module Main where
-- Compile with ghc -O2 -o Main MainV2.hs
-- Compile with ghc -O2 -o Main -threaded -rtsopts -with-rtsopts=-N MainV2.hs

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
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
  go taskCount
  where
    go :: Int -> IO ()
    go 0 = return ()
    go taskCount = do
      chan <- newChan
      forkIO $ do
        threadDelay sleepMicroseconds
        writeChan chan ()
      go $ taskCount - 1
      readChan chan