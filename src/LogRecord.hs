{-# LANGUAGE FlexibleContexts #-}
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
import Data.Time.Clock
import Data.Time.Format

import Types2048

emptyLogRecord :: LogRecord
emptyLogRecord = []

warn :: (MonadReader UTCTime m, MonadWriter LogRecord m) => String -> m ()
warn xs = mkLogMessage "WARN" xs >>= output

info :: (MonadReader UTCTime m, MonadWriter LogRecord m) => String -> m ()
info xs = mkLogMessage "INFO" xs >>= output

dbug :: (MonadReader UTCTime m, MonadWriter LogRecord m) => String -> m ()
dbug xs = mkLogMessage "DBUG" xs >>= output

output :: (MonadReader UTCTime m, MonadWriter LogRecord m) => String -> m ()
output x = tell [x]

debug :: (MonadReader UTCTime m, MonadWriter LogRecord m) => String -> m ()
debug = dbug

mkLogMessage :: (MonadReader UTCTime m, MonadWriter LogRecord m) => String -> String -> m String
mkLogMessage lvl msg = do
    time <- formatTimestamp
    return (lvl ++ "|" ++ time ++ "|" ++ msg)

formatTimestamp :: (MonadReader UTCTime m, MonadWriter LogRecord m) => m String
formatTimestamp = formatTime defaultTimeLocale "%T" <$> ask

logSeparator :: (MonadReader UTCTime m, MonadWriter LogRecord m) => m ()
logSeparator = output ""
