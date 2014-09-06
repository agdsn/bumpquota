{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (unlines)

import Control.Applicative ( (<$>)
                           , (<*>)
                           , (<|>)
                           , (<*)
                           , optional
                           )
import Data.Attoparsec.Text ( char
                            , count
                            , decimal
                            , endOfInput
                            , endOfLine
                            , many1
                            , parseOnly
                            , skipSpace
                            , takeTill
                            )
import qualified Data.Attoparsec.Text as AP
import Data.Char (isSpace)
import Data.Text ( Text ()
                 , pack
                 , strip
                 , unpack
                 , unlines
                 )
import Data.Maybe (fromMaybe)
import Data.Monoid ( Monoid ( mempty
                            , mappend
                            )
                   , (<>)
                   )
import System.Process (readProcess)

import Options.Applicative ( Parser ()
                           , ParserInfo ()
                           , execParser
                           , fullDesc
                           , header
                           , help
                           , helper
                           , info
                           , long
                           , metavar
                           , option
                           , progDesc
                           , strOption
                           )

type Login = Text
data QuotaConfig = QuotaConfig { blockSoftCfg :: Maybe Integer
                               , blockHardCfg :: Maybe Integer
                               , inodeSoftCfg :: Maybe Integer
                               , inodeHardCfg :: Maybe Integer
                               , filesystem :: String
                               }

config' :: Parser QuotaConfig
config' = QuotaConfig
          <$> optional (option $ long "block-soft"
                        <> metavar "BLOCKSOFT"
                        <> help "Block soft limit")
          <*> optional (option $ long "block-hard"
                        <> metavar "BLOCKHARD"
                        <> help "Block hard limit")
          <*> optional (option $ long "inode-soft"
                        <> metavar "INODESOFT"
                        <> help "Inode soft limit")
          <*> optional (option $ long "inode-hard"
                        <> metavar "INODEHARD"
                        <> help "Inode hard limit")
          <*> strOption (long "filesystem"
                         <> metavar "FILESYSTEM"
                         <> help "Filesystem to bump quotas on")

config :: ParserInfo QuotaConfig
config = info (helper <*> config')
         (fullDesc
          <> progDesc (concat ["Bump quotas on a given filesystem. "
                              , "Exising higher quotas will be preserved, "
                              , "as will users without quotas."])
          <> header "bumpquota - bump quotas for everyone")

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

parseLogin :: AP.Parser Login
parseLogin = takeTill isSpace

parseQuota :: AP.Parser (Login, Quota)
parseQuota = do
  login <- parseLogin
  skipSpace
  _ <- count 2 $ char '-' <|> char '+'
  skipSpace
  quota <- Quota                 -- repquota output, ignore grace times
           <$> num              -- blockUsed
           <*> num              -- blockSotf
           <*> (num <* num)     -- blockHard, blockGrace
           <*> num              -- inodeUsed
           <*> num              -- inodeSoft
           <*> (num <* (decimal :: AP.Parser Integer)) -- inodeHard, inodeGrace
  _ <- endOfLine <|> endOfInput
  return (login, quota)
  where num = decimal <* skipSpace


handleQuota' :: QuotaConfig -> Login -> Quota -> IO ()
handleQuota' cfg login quota = do
  let liftLimit ctor ctor' = fromMaybe (ctor quota) $ ctor' cfg
      quota' = quota <> mempty { blockSoft = liftLimit blockSoft blockSoftCfg
                               , blockHard = liftLimit blockHard blockHardCfg
                               , inodeSoft = liftLimit inodeSoft inodeSoftCfg
                               , inodeHard = liftLimit inodeHard inodeHardCfg
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
  cfg <- execParser config
  repQuota <- readProcess "repquota" ["-p", filesystem cfg] ""
  let lquota = filter (/="") . drop 5 . map (strip . pack) . lines $ repQuota
  let eQ = parseOnly (many1 parseQuota) $ unlines lquota
  case eQ of
    Left e -> putStrLn $ "Parse error: `" <> e <> "'"
    Right quotas -> mapM_ (uncurry $ handleQuota' cfg) quotas
