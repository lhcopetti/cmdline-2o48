module LogRecord
    ( output
    , emptyLogRecord
    ) where

import Control.Monad.Writer

import Types2048

output :: String -> M2048 ()
output x = tell [x]

emptyLogRecord :: LogRecord
emptyLogRecord = []

