module Types2048
    ( M2048
    , EM2048
    , Game2048 (..)
    , GameEnded (..)
    , Board2048 (..)
    , runM2048
    , runM2048Gen
    , erunM2048
    , erunM2048Gen
    , LogRecord
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.Writer
import Data.Bifunctor

import Data.Time.Clock (UTCTime, getCurrentTime)
import DirectionCounter
import System.Random

newtype Board2048 = Board2048 [[Int]]
    deriving (Show, Eq)

data Game2048 = Game2048 
    { board :: Board2048
    , gen   :: StdGen
    , count :: DirectionCounter
    , logR   :: [String]
    , devel :: Bool
    }

data GameEnded  = YouWon Game2048
                | YouLose Game2048

type LogRecord = [String]

type M2048 = ReaderT UTCTime (WriterT LogRecord (State StdGen))
type EM2048 a = EitherT GameEnded M2048 a

runM2048 :: Game2048 -> M2048 Game2048 -> IO Game2048
runM2048 genSupplier = runM2048Gen (gen genSupplier)

runM2048Gen :: StdGen -> M2048 Game2048 -> IO Game2048
runM2048Gen gen game = do
    currTime <- getCurrentTime
    let ((eg', w), gen') = runState (runWriterT (runReaderT game currTime)) gen
    return (updateGameInternals w gen' eg')

erunM2048 :: Game2048 -> EM2048 Game2048 -> IO (Either GameEnded Game2048)
erunM2048 genSupplier = erunM2048Gen (gen genSupplier)

erunM2048Gen :: StdGen -> EM2048 Game2048 -> IO (Either GameEnded Game2048)
erunM2048Gen gen game = do
    currTime <- getCurrentTime
    let ((eg', w), gen') = runState (runWriterT (runReaderT (runEitherT game) currTime)) gen
        updateGameEnded = updateGameEndedInternals w gen
        updateGame = updateGameInternals w gen
    return . bimap updateGameEnded updateGame $ eg'

updateGameEndedInternals :: LogRecord -> StdGen -> GameEnded -> GameEnded
updateGameEndedInternals log gen (YouWon g) = YouWon (updateGameInternals log gen g)
updateGameEndedInternals log gen (YouLose g) = YouLose (updateGameInternals log gen g)

updateGameInternals :: LogRecord -> StdGen -> Game2048 -> Game2048
updateGameInternals log' gen' g = g { logR = logR g ++ log', gen = gen' }
