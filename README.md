# bumpquota
Automatically bump quotas for everyone, ignoring users without quota, and users with quotas already higher than the new default quota. Uses `repquota`, and generates a batch file suitable for input to `setquota -b`.

## building
Use [stack](https://github.com/commercialhaskell/stack).

    stack build

## usage

    bumpquota --block-soft=1000000  --block-hard=1100000 --filesystem=/home | setquota -b /home

will raise any block quota on /home to the given amounts, leaving any users with higher quotas or users without quotas untouched. Also supports inode limits.