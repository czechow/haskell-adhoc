module Channel where

import Control.Concurrent


type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

data Ch a = Ch (MVar (Stream a)) (MVar (Stream a))

newCh :: IO (Ch a)
newCh = do
  emptyStream <- newEmptyMVar
  mvRead <- newMVar emptyStream
  mvWrite <- newMVar emptyStream
  return $ Ch mvRead mvWrite

writeCh :: Ch a -> a -> IO ()
writeCh (Ch _ mvWrite) x = do
  currStream <- takeMVar mvWrite
  emptyStream <- newEmptyMVar
  putMVar currStream $ Item x emptyStream
  putMVar mvWrite emptyStream

readCh :: Ch a -> IO a
readCh (Ch mvRead _) = do
  currStream <- takeMVar mvRead
  (Item x nextStream) <- readMVar currStream
  putMVar mvRead nextStream
  return x

dupCh :: Ch a -> IO (Ch a)
dupCh (Ch _ mvWrite) = do
  wrPtr <- readMVar mvWrite
  mvRead' <- newMVar wrPtr
  return $ Ch mvRead' mvWrite
