module Main where
-- Compile with ghc -O2 -o Main MainV4.hs
-- Compile with ghc -O2 -o Main -threaded -rtsopts -with-rtsopts=-N MainV4.hs

import Control.Monad
import Control.Concurrent
import Control.Concurrent.QSemN as QSemN
import Control.Concurrent.Chan as Chan
import Control.Concurrent.MVar as MVar
import Data.IORef as IORef
import System.Environment (getArgs)

-- ghc -O2 -o Main -prof MainV7.hs
-- time ./Main 1000000 1 +RTS -hc -p
-- time ./Main 1000000 1000 +RTS -hc -p
main :: IO ()
main = do
  args <- getArgs
  let variant = head args
      [taskCount, sleepMilliseconds] = map read $ tail args
      sleepMicroseconds = sleepMilliseconds * 1000

  putStrLn $ "Running variant " <> variant <> " with " <> show taskCount <> " task(s)."
  putStrLn $ "Sleeping " <> show sleepMilliseconds <> "ms for each task."

  case variant of
    "chan1" -> chanVariant1 taskCount sleepMicroseconds
    "chan2" -> chanVariant2 taskCount sleepMicroseconds
    "sem" -> semVariant taskCount sleepMicroseconds
    "mvar" -> mvarVariant taskCount sleepMicroseconds
    "ioref" -> ioRefVariant taskCount sleepMicroseconds
    _ -> putStrLn "Variant unknown."

chanVariant1 :: Int -> Int -> IO ()
chanVariant1 0 _ = return ()
chanVariant1 taskCount sleepMicroseconds = do
  chan <- Chan.newChan
  forkIO $ do
    threadDelay sleepMicroseconds
    Chan.writeChan chan ()
  chanVariant1 (taskCount - 1) sleepMicroseconds
  Chan.readChan chan

chanVariant2 :: Int -> Int -> IO ()
chanVariant2 taskCount sleepMicroseconds = do
  chans <- forM [1..taskCount] $ \_ -> do
    chan <- Chan.newChan
    forkIO $ do
      threadDelay sleepMicroseconds
      Chan.writeChan chan ()
    return chan
  
  forM_ chans $ \c -> Chan.readChan c

semVariant :: Int -> Int -> IO ()
semVariant taskCount sleepMicroseconds = do
  doneCount <- QSemN.newQSemN 0

  forM_ [1..taskCount] $ \_ -> forkIO $ do
    threadDelay sleepMicroseconds
    QSemN.signalQSemN doneCount 1

  QSemN.waitQSemN doneCount taskCount

mvarVariant :: Int -> Int -> IO ()
mvarVariant taskCount sleepMicroseconds = do
  vars <- forM [1..taskCount] $ \_ -> do
    var <- MVar.newEmptyMVar
    forkIO $ do
      threadDelay sleepMicroseconds
      MVar.putMVar var ()
    return var
  
  forM_ vars MVar.readMVar

ioRefVariant :: Int -> Int -> IO ()
ioRefVariant taskCount sleepMicroseconds = do
  doneCount <- IORef.newIORef 0

  forM_ [1..taskCount] $ \_ -> forkIO $ do
    threadDelay sleepMicroseconds
    IORef.atomicModifyIORef' doneCount (\x -> (x + 1, ()))

  threadDelay sleepMicroseconds
  waitFinish doneCount
  where
    waitFinish :: IORef Int -> IO ()
    waitFinish doneCount = do
      threadDelay 1000
      count <- IORef.readIORef doneCount
      when (count < taskCount) $ waitFinish doneCount

-- Warum langsamer je länger warten?
-- Warum mehr Memory je länger warten?
