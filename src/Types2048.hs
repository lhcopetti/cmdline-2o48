{-# LANGUAGE RecordWildCards #-}
module Types2048
    ( M2048
    , Game2048 (..)
    , Board2048 (..)
    , runM2048
    , runM2048Gen
    , LogRecord
    ) where

import Control.Monad.State
import Control.Monad.Writer
import DirectionCounter
import System.Random

data Board2048 = Board2048 [[Int]]
    deriving (Show, Eq)

data Game2048 = Game2048 
    { board :: Board2048
    , gen   :: StdGen
    , count :: DirectionCounter
    , logR   :: [String]
    }


type LogRecord = [String]

type M2048 a = WriterT LogRecord (State StdGen) a

runM2048 :: Game2048 -> M2048 Game2048 -> Game2048
runM2048 genSupplier = runM2048Gen (gen genSupplier)

runM2048Gen :: StdGen -> M2048 Game2048 -> Game2048
runM2048Gen gen game =
    let ((g', w), gen') = runState (runWriterT game) gen
     in (g' { logR = logR g' ++ w, gen = gen' })

