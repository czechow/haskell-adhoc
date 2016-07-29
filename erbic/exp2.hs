{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Exception
import Network.Socket
import Data.Map.Strict as M


data SSData a = SSData { name :: String
                       , svcData :: Maybe (ThreadId, MVar ())
                       , connData :: MVar (M.Map ThreadId (MVar ())) }


class Show s => Service s where
  ssOpen :: IO s
  ssClose :: s -> IO ()
  ssAccept :: s -> IO (s, String)


  newSS :: HostName -> Int -> IO (SSData s)
  newSS _ _ = do
    mvConnData <- newMVar M.empty
    return $ SSData "SockSrvName" Nothing mvConnData


  startSS :: SSData s -> IO (SSData s)
  startSS ssd =
    bracketOnError (ssOpen :: IO s) (ssClose) $ \s ->
      bracketOnError (ffork $ ssSocketAccLoop s $ connData ssd)
                     (stopThread 0) $
                     \(tid, mv) -> return $ ssd { svcData = Just (tid, mv) }


  stopSS :: Int -> SSData s -> IO ()
  stopSS t ssd =
    mask_ $ case svcData ssd of
      Just (tid, mv) -> do
        putStrLn $ "Stopping service " ++ name ssd ++ " " ++ show tid
        stopThread t (tid, mv)
        putStrLn $ "Service thread " ++ show tid ++ " stopped"
        tidsMvsMap <- readMVar $ connData ssd
        putStrLn $ "Now stopping connections: " ++ (show $ M.keys tidsMvsMap)
        stopThreadPool t $ connData ssd
        putStrLn $ "All connections stopped"
      Nothing -> do
        putStrLn $ "Service was not started, nothing to stop"


  isSSRunning :: SSData s -> IO Bool
  isSSRunning ssd = case svcData ssd of
    Just (_, mv) -> tryReadMVar mv >>= \case
      Just _ -> return False
      Nothing -> return True
    Nothing -> return False

  -- default implementations...


  ssSocketAccLoop :: s -> MVar (M.Map ThreadId (MVar ())) -> IO ()
  ssSocketAccLoop s mvTids = do
    bracketOnError (ssAccept s) (ssClose . fst) $
       \(s', sa') -> mask_ $ do _ <- cffork (ssServeConnection s' sa') mvTids
                                return ()
    ssSocketAccLoop s mvTids


  ssServeConnection :: s -> String -> IO ()
  ssServeConnection s sa = do
    finally (do tid <- myThreadId
                putStrLn $ "New connection from " ++ show sa ++
                           " on " ++ show s ++
                           " served in " ++ show tid
                doSrv)
            (do ssClose s
                tid <- myThreadId
                putStrLn $ "Closed connection on " ++ show s ++
                           " in " ++ show tid)
      where
        doSrv = threadDelay $ 10 * 1000 * 1000

-------------------------------------------------------------------------------
--                                Instances
-------------------------------------------------------------------------------

instance Service Socket where
  ssOpen =
      bracketOnError
        (socket AF_INET Stream defaultProtocol)
        (\s -> do close s
                  putStrLn $ "openSock: Socket closed " ++ show s)
        (\s -> do
            setSocketOption s ReuseAddr 1
            hostAddr <- inet_addr "127.0.0.1"
            putStrLn $ "Acquired socket " ++ show s
            bind s (SockAddrInet 2222 hostAddr)
            listen s 5
            threadDelay $ 1 * 1000 * 1000
            return s)
  ssClose s = close s
  ssAccept s = do (s', sa') <- accept s
                  return (s', show sa')

-------------------------------------------------------------------------------
--                                fork helpers
-------------------------------------------------------------------------------

ffork :: IO () -> IO (ThreadId, MVar ())
ffork action = do
  mv <- newEmptyMVar :: IO (MVar ())
  tid <- forkIOWithUnmask $
         \unmask -> finally (unmask action)
                            (putMVar mv ())
  return (tid, mv)

cffork :: IO () -> MVar (M.Map ThreadId (MVar ())) -> IO (ThreadId)
cffork action mvTids =
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


stopThreadPool :: Int -> MVar (M.Map ThreadId (MVar ())) -> IO ()
stopThreadPool _ mvThrInfos = do
  tidsMvsMap <- readMVar mvThrInfos
  mapM_ killThread $ M.keys tidsMvsMap
  mapM_ readMVar $ M.elems tidsMvsMap


stopThread :: Int -> (ThreadId, MVar ()) -> IO ()
stopThread _ (tid, mv) = do
  killThread tid
  readMVar mv




main :: IO ()
main = do
  ssd <- newSS "X" 12 :: IO (SSData Socket)
  ssd' <- startSS ssd
  case svcData ssd' of
    Just (tid, _) -> do
      putStrLn $ "Service started & running with " ++ show tid
      putStrLn $ "Press enter to exit"
      _ <- getLine
      stopSS 10 ssd'
      putStrLn $ "All clean, main thread terminated"
      return ()
    Nothing -> do
      putStrLn $ "Service could not be started"
