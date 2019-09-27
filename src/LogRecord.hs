module LogRecord
    ( emptyLogRecord
    , warn
    , info
    , dbug
    , debug
    , logSeparator
    ) where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Time.Format

import Types2048


emptyLogRecord :: LogRecord
emptyLogRecord = []

warn :: String -> M2048 ()
warn xs = mkLogMessage "WARN" xs >>= output

info :: String -> M2048 ()
info xs = mkLogMessage "INFO" xs >>= output

dbug :: String -> M2048 ()
dbug xs = mkLogMessage "DBUG" xs >>= output

output :: String -> M2048 ()
output x = tell [x]

debug :: String -> M2048 ()
debug = dbug

mkLogMessage :: String -> String -> M2048 String
mkLogMessage lvl msg = do
    time <- formatTimestamp
    return (lvl ++ "|" ++ time ++ "|" ++ msg)

formatTimestamp :: M2048 String
formatTimestamp = liftM (formatTime defaultTimeLocale "%T") ask

logSeparator :: M2048 ()
logSeparator = output ""
