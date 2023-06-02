module Main where
-- Compile with ghc -O2 -o Main MainV4.hs
-- Compile with ghc -O2 -o Main -threaded -rtsopts -with-rtsopts=-N MainV4.hs

import Control.Monad
import Control.Concurrent
import Control.Concurrent.QSemN as QSemN
import System.Environment (getArgs)

sleepMicroseconds :: Int
sleepMicroseconds = 10_000_000

getTaskCount :: IO Int
getTaskCount = do
  args <- liftM (concatMap reads) getArgs
  when (null args) $ error "Expected number of tasks as an argument."
  return . fst $ head args

main :: IO ()
main = do
  taskCount <- getTaskCount
  putStrLn $ "Running " <> show taskCount <> " task(s)"
  semaphore <- QSemN.newQSemN 0
  forM [1..taskCount] $ \_ -> forkIO $ do
    threadDelay sleepMicroseconds
    QSemN.signalQSemN semaphore 1
  QSemN.waitQSemN semaphore taskCount
