module LogRecord
    ( emptyLogRecord
    , warn
    , info
    , dbug
    , debug
    , logSeparator
    ) where

import Control.Monad.Writer

import Types2048


emptyLogRecord :: LogRecord
emptyLogRecord = []

warn :: String -> M2048 ()
warn xs = output $ "WARN|" ++ xs

info :: String -> M2048 ()
info xs = output $ "INFO|" ++ xs

dbug :: String -> M2048 ()
dbug xs = output $ "DBUG|" ++ xs

debug :: String -> M2048 ()
debug = dbug

output :: String -> M2048 ()
output x = tell [x]

logSeparator :: M2048 ()
logSeparator = output ""
