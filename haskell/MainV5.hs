module Main where
-- Compile with ghc -O2 -o Main MainV3.hs
-- Compile with ghc -O2 -o Main -threaded -rtsopts -with-rtsopts=-N MainV3.hs

import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.STM as STM
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

  doneCount <- STM.atomically $ STM.newTVar (0 :: Int)

  forM [1..taskCount] $ \_ -> forkIO $ do
    threadDelay sleepMicroseconds
    STM.atomically $ STM.modifyTVar doneCount (+ 1)

  STM.atomically $ do
    done <- STM.readTVar doneCount
    when (done < taskCount) $ STM.retry
