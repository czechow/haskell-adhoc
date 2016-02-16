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

-- we want to implement channels that allow simultaneous read and write

data Channel a = Channel (MVar (MVar (Node a))) (MVar (MVar (Node a)))
data Node a = Elem a (MVar (Node a))
            | Hole


newChannel :: IO (Channel a)
newChannel = do
  mvHole <- newMVar Hole
  mvRead <- newMVar mvHole
  mvWrite <- newMVar mvHole
  return $ Channel mvRead mvWrite

writeChannel :: Channel a -> a -> IO ()
writeChannel (Channel _ mvWrite) x = do
  mvWrtPtr <- takeMVar mvWrite
  _ <- takeMVar mvWrtPtr
  newHole <- newMVar Hole
  let newNode = (Elem x newHole)
  putMVar mvWrtPtr newNode
  putMVar mvWrite newHole

dumpChannel :: Show a => Channel a -> IO ()
dumpChannel (Channel mvRead _) = do
  mvReadPtr <- readMVar mvRead
  dumpNode mvReadPtr
    where
      dumpNode :: Show a => MVar (Node a) -> IO ()
      dumpNode mvNode = do
        node <- readMVar mvNode
        case node of
          Hole -> putStrLn "[Hole]"
          (Elem x mv) -> do putStr ("[" ++ show x ++ "]->")
                            dumpNode mv
        return ()

readChannel :: Channel a -> IO (Maybe a)
readChannel (Channel mvRead _) = do
  mvReadPtr <- readMVar mvRead
  node <- readMVar mvReadPtr
  case node of
    (Elem x nextNode) -> do
      _ <- takeMVar mvRead
      putMVar mvRead nextNode
      return $ Just x
    Hole -> return $ Nothing
