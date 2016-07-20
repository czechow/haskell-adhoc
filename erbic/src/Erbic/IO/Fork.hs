module Erbic.IO.Fork where

import Control.Concurrent
import Control.Exception
import Data.Map.Strict as M


-- FIXME: implement stop timeouts (timeout composable?)

type ThreadInfo = (ThreadId, MVar ())
type ThreadPoolInfo = M.Map ThreadId (MVar ())
type TimeoutMs = Int


tfork :: IO () -> IO ThreadInfo
tfork action = do
  mv <- newEmptyMVar :: IO (MVar ())
  tid <- forkIOWithUnmask $
         \unmask -> finally (unmask action)
                            (putMVar mv ())
  return (tid, mv)


tpfork :: IO () -> MVar ThreadPoolInfo -> IO (ThreadId)
tpfork action mvTids =
  forkIOWithUnmask $ \unmask -> do bracket_ startup
                                            finish $
                                            unmask action
  where
    startup = do
      mv <- newEmptyMVar :: IO (MVar ())
      tid <- myThreadId
      uninterruptibleMask_ $
        modifyMVar_ mvTids $ \tis -> return $ M.insert tid mv tis

    finish = do
      tid <- myThreadId
      uninterruptibleMask_ $
        modifyMVar_ mvTids $ \tis -> case M.lookup tid tis of
          Just mv -> do
            putMVar mv ()
            return $ M.delete tid tis
          Nothing -> return tis


stopThreadPool :: TimeoutMs -> MVar ThreadPoolInfo -> IO ()
stopThreadPool _ mvThrInfos = do
  tidsMvsMap <- readMVar mvThrInfos
  mapM_ killThread $ M.keys tidsMvsMap
  mapM_ readMVar $ M.elems tidsMvsMap


stopThread :: TimeoutMs -> ThreadInfo -> IO ()
stopThread _ (tid, mv) = do
  killThread tid
  readMVar mv
