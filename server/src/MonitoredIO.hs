module MonitoredIO where

import Prelude hiding (putChar, putStr, putStrLn, print, getChar, getLine, readLn)

import Control.Concurrent.STM
import Control.Concurrent.Async (race)

import Test.IOTasks

runMonitoredIOon :: [String] -> MonitoredIO a -> IO a
runMonitoredIOon xs p = do
  inputs <- newTQueueIO
  mapM_ (atomically . writeTQueue inputs) $ unlines xs
  runMonitoredIO p inputs

abortableWith :: Char -> MonitoredIO a -> IO (Maybe a)
abortableWith abortSymbol p = do
  inputs <- newTQueueIO
  let
    waitForInput = do
      sym <- getChar
      if sym == abortSymbol
        then pure ()
        else do
          atomically $ writeTQueue inputs sym
          waitForInput
  res <- race waitForInput $ runMonitoredIO p inputs
  pure $ either (const Nothing) Just res

newtype MonitoredIO a = MonitoredIO { runMonitoredIO :: TQueue Char -> IO a }

instance Functor MonitoredIO where
  fmap f io = MonitoredIO $ fmap f . runMonitoredIO io

instance Applicative MonitoredIO where
  pure = MonitoredIO . const . pure
  MonitoredIO f <*> MonitoredIO g = MonitoredIO $ \vs -> f vs <*> g vs

instance Monad MonitoredIO where
  MonitoredIO f >>= g = MonitoredIO $ \vs -> f vs >>= ((`runMonitoredIO` vs) . g)

instance MonadTeletype MonitoredIO where
  putChar = MonitoredIO . const . putStrLn . ("<char>" ++) . pure
  getChar = MonitoredIO $ atomically . readTQueue

  putStr = MonitoredIO . const . putStrLn . ("<str>" ++)
  putStrLn = MonitoredIO . const . putStrLn . ("<line>" ++)
  hSetBuffering m = MonitoredIO . const . hSetBuffering m

p :: MonadTeletype io => io ()
p = loop 0
  where
    loop n = do
      putStr "First number or 0 to exit: "
      x <- readLn
      if x == 0
        then do
          putStrLn "Exiting program"
          putStr "The number of additions performed was: "
          print n
        else do
          putStr "Second number: "
          y <- readLn
          putStr ("The sum of " ++ show x ++ " and " ++ show y ++ " is ")
          print (x + y)
          loop (n + 1)
