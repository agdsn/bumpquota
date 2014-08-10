{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (unlines)

import Control.Applicative ( (<$>)
                           , (<*>)
                           , (<|>)
                           , (<*)
                           )
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Default ( Default (..)
                    , def
                    )
import Data.Text ( Text ()
                 , pack
                 , strip
                 , unpack
                 , unlines
                 )
import Data.Monoid ( Monoid ( mempty
                            , mappend
                            )
                   , (<>)
                   )
import System.Process (readProcess)

type Login = Text
data QuotaConfig = QuotaConfig { filesystem :: String
                               , soft :: Integer
                               , hard :: Integer
                               , slack :: Integer
                               }

instance Default QuotaConfig where
  def = QuotaConfig { filesystem = "/home"
                    , soft = 1000000
                    , hard = 1100000
                    , slack = 100000
                    }

data Quota =  Quota { blockUsed :: Integer
                    , blockSoft :: Integer
                    , blockHard :: Integer
                    , inodeUsed :: Integer
                    , inodeSoft :: Integer
                    , inodeHard :: Integer
                    }
              deriving Show

instance Monoid Quota
  where mempty = Quota { blockUsed = 0
                       , blockSoft = 0
                       , blockHard = 0
                       , inodeUsed = 0
                       , inodeSoft = 0
                       , inodeHard = 0
                       }
        mappend l r = Quota { blockUsed = lift blockUsed min
                            , blockSoft = lift blockSoft max
                            , blockHard = lift blockHard max
                            , inodeUsed = lift inodeUsed min
                            , inodeSoft = lift inodeSoft max
                            , inodeHard = lift inodeHard max
                            }
          where lift ctor cmp
                  | ctor l == 0 = 0
                  | ctor r == 0 = 0
                  | otherwise = cmp (ctor l) (ctor r)

parseLogin :: Parser Login
parseLogin = takeTill isSpace

parseQuota :: Parser (Login, Quota)
parseQuota = do
  login <- parseLogin
  skipSpace
  _ <- count 2 $ char '-' <|> char '+'
  skipSpace
  quota <- Quota <$> num <*> num <*> (num <* num) <*> num <*> num <*> (num <* (decimal :: Parser Integer))
  _ <- endOfLine <|> endOfInput
  return (login, quota)
  where num = decimal <* skipSpace


handleQuota :: Login -> Quota -> IO ()
handleQuota = handleQuota' def

handleQuota' :: QuotaConfig -> Login -> Quota -> IO ()
handleQuota' cfg login quota = do
  let quota' = quota <> mempty { blockSoft = soft cfg
                               , blockHard = hard cfg
                               }

  putStrLn $ formatQuota login quota'

formatQuota :: Login -> Quota -> String
formatQuota login quota = unwords [ unpack login
                                  , show . blockSoft $ quota
                                  , show . blockHard $ quota
                                  , show . inodeSoft $ quota
                                  , show . inodeHard $ quota
                                  ]

main :: IO ()
main = do
  let cfg = def
  repQuota <- readProcess "repquota" ["-p", filesystem cfg] ""
  let lquota = filter (/="") . drop 5 . map (strip . pack) . lines $ repQuota
  let eQ = parseOnly (many1 parseQuota) $ unlines lquota
  case eQ of
    Left e -> putStrLn $ "Parse error: `" <> e <> "'"
    Right quotas -> mapM_ (uncurry $ handleQuota' cfg) quotas
