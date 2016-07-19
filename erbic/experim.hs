{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

import System.Timeout
import System.IO
import Control.Concurrent
import Control.Exception
import Network.Socket
import Control.Monad
import GHC.IO.Exception
import Data.IORef
import Data.Map.Strict as M
import System.Random
import GHC.IO (unsafeUnmask)
import System.Posix.Signals



data SockServiceData = SockServiceData
                       { name :: String
                       , svcData :: Maybe (ThreadId, MVar ())
                       , connData :: MVar (M.Map ThreadId (MVar ()))
                       }


newSockService :: HostName -> Int -> IO SockServiceData
newSockService _ _ = do
  mvConnData <- newMVar M.empty
  return $ SockServiceData "SockSrvName" Nothing mvConnData


startSockService :: SockServiceData -> IO SockServiceData
startSockService ssd = do
  bracketOnError (openSock) (close) $ \s ->
    bracketOnError (ffork $ socketAccLoop s $ connData ssd)
                   (stopThread 0) $
                   \(tid, mv) -> return $ ssd { svcData = Just (tid, mv) }


-- FIXME: timeout implemented incorrectly
stopSockService :: Int -> SockServiceData -> IO ()
stopSockService t ssd =
  mask_ $
    case svcData ssd of
      Just (tid, mv) -> do
        putStrLn $ "Stopping service " ++ show tid
        stopThread t (tid, mv)
        putStrLn $ "Service thread " ++ show tid ++ " stopped"
        tidsMvsMap <- readMVar $ connData ssd
        putStrLn $ "Now stopping connections: " ++ (show $ M.keys tidsMvsMap)
        stopThreadPool t $ connData ssd
        putStrLn $ "All connections stopped"
      Nothing -> do
        putStrLn $ "Service was not started, nothing to stop"


isSockServiceRunning :: SockServiceData -> IO Bool
isSockServiceRunning ssd = case svcData ssd of
  Just (_, mv) -> tryReadMVar mv >>= \case
    Just _ -> return False
    Nothing -> return True
  Nothing -> return False


socketAccLoop :: Socket -> MVar (M.Map ThreadId (MVar ())) -> IO ()
socketAccLoop s mvTids = do
  bracketOnError (accept s) (close . fst) $
       \(s', sa') -> mask_ $ do _ <- cffork (serveConnection s' sa') mvTids
                                return ()
  socketAccLoop s mvTids


serveConnection :: Socket -> SockAddr -> IO ()
serveConnection s sa = do
  finally (do tid <- myThreadId
              putStrLn $ "New connection from " ++ show sa ++
                         " on " ++ show s ++
                         " served in " ++ show tid
              doSrv)
          (do close s
              tid <- myThreadId
              putStrLn $ "Closed connection on " ++ show s ++
                         " in " ++ show tid)
  where
    doSrv = threadDelay $ 10 * 1000 * 1000




-- Are we happy with such interface?
-- All threads start with parent mask defined policy
-- Then threads unmask actions as they need
-- The semantics is that the parent decides on mask policy

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

-- FIXME: ignoring timeout for now => will this compose with timeout?
stopThreadPool :: Int -> MVar (M.Map ThreadId (MVar ())) -> IO ()
stopThreadPool _ mvThrInfos = do
  tidsMvsMap <- readMVar mvThrInfos
  mapM_ killThread $ M.keys tidsMvsMap
  mapM_ readMVar $ M.elems tidsMvsMap

-- FIXME: ignoring timeout for now => will this compose with timeout?
stopThread :: Int -> (ThreadId, MVar ()) -> IO ()
stopThread _ (tid, mv) = do
  killThread tid
  readMVar mv




main :: IO ()
main = do
  ssd <- newSockService "X" 12
  ssd' <- startSockService ssd
  case svcData ssd' of
    Just (tid, _) -> do
      putStrLn $ "Service started & running with " ++ show tid
      putStrLn $ "Press enter to exit"
      _ <- getLine
      stopSockService 10 ssd'
      putStrLn $ "All clean, main thread terminated"
      return ()
    Nothing -> do
      putStrLn $ "Service could not be started"




openSock :: IO Socket
openSock =
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
