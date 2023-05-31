module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment (getArgs)

sleepMicroseconds :: Int
sleepMicroseconds = 10 * 1000 * 1000

task :: Chan () -> IO () 
task chan = do
  threadDelay sleepMicroseconds
  writeChan chan ()

startTask :: IO (Chan ())
startTask = do
  chan <- newChan
  forkIO $ task chan
  return chan

waitTask :: Chan () -> IO ()
waitTask = readChan

getTaskCount :: IO Int
getTaskCount = do
  args <- liftM (concatMap reads) getArgs
  when (null args) $ error "Expected number of tasks as an argument."
  return . fst $ head args

main :: IO ()
main = do
  taskCount <- getTaskCount
  putStrLn $ "Running " <> show taskCount <> " task(s)"
  chans <- forM [1..taskCount] $ \_ -> startTask
  mapM_ waitTask chans