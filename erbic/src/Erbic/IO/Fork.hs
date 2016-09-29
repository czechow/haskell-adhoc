module Erbic.IO.Fork where

import Control.Concurrent
import Control.Exception
import Data.Map.Strict as M
import Data.IORef

-- FIXME: implement stop timeouts (timeout composable?)

type ThreadInfo = (ThreadId, MVar ())
type ThreadPoolInfo = M.Map ThreadId (MVar ())
type TimeoutMs = Int


tfork :: IO () -> IO ThreadInfo
tfork action = do
  mv <- newEmptyMVar :: IO (MVar ())
  tid <- mask_ $ forkIOWithUnmask $ \unmask -> finally (unmask action)
                                                       (putMVar mv ())
  return (tid, mv)


tpfork :: IO () -> IORef ThreadPoolInfo -> IO (ThreadId)
tpfork action mvTids =
  forkIOWithUnmask $ \unmask -> do bracket_ startup
                                            finish $
                                            unmask action
  where
    startup = do
      mv <- newEmptyMVar :: IO (MVar ())
      tid <- myThreadId
      uninterruptibleMask_ $
        modifyIORef' mvTids $ \tis -> M.insert tid mv tis

    finish = do
      tid <- myThreadId
      mmv <- atomicModifyIORef' mvTids $ \tis -> let ret = M.lookup tid tis
                                                 in (M.delete tid tis, ret)
      case mmv of
        Just mv -> putMVar mv ()
        Nothing -> return ()


stopThreadPool :: TimeoutMs -> IORef ThreadPoolInfo -> IO ()
stopThreadPool _ mvThrInfos = do
  tidsMvsMap <- readIORef mvThrInfos
  mapM_ killThread $ M.keys tidsMvsMap
  mapM_ readMVar $ M.elems tidsMvsMap


stopThread :: TimeoutMs -> ThreadInfo -> IO ()
stopThread _ (tid, mv) = do
  killThread tid
  readMVar mv
