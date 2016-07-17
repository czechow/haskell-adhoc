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
import System.Random
import GHC.IO (unsafeUnmask)


{-
canForkIOFail :: IO ()
canForkIOFail = mapM_ (idleThread) [1..]



idleThread :: Int -> IO ThreadId
idleThread n = do
  putStrLn $ "Forked " ++ show n
  forkFinally (threadDelay $ 60 * 1000 * 1000)
              (\_ -> putStrLn $ "Finalizer in thread called")

main :: IO ()
main = finally canForkIOFail $ do
  putStrLn $ "Finally handler called"
-}


ffork :: IO () -> IO (ThreadId, MVar ())
ffork action = do
  mv <- newEmptyMVar :: IO (MVar ())
  tid <- forkIO $ finally action (putMVar mv ())
  return (tid, mv)

socketAcc :: Socket -> MVar [(ThreadId, MVar ())] -> IO ()
socketAcc s mvTids = do
  bracketOnError (accept s) (close . fst) $ \(s', sa') ->
    mask_ $ do
      (tid, mv) <- ffork $ serveConnection s' sa'
      modifyMVar_ mvTids $ \tis -> let !tis' = (tid, mv) : tis
                                   in return tis'
  socketAcc s mvTids


serveConnection :: Socket -> SockAddr -> IO ()
serveConnection s sa =
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
    doSrv = threadDelay $ 4 * 1000 * 1000


main :: IO ()
main = do
  tis <- newMVar []
  bracket openSock
          (\s -> do close s
                    putStrLn $ "Closed socket " ++ show s
                    ctis <- readMVar tis
                    putStrLn $ "Remaining Threads: " ++ show (map fst ctis)) $
          \s -> socketAcc s tis


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
            threadDelay 5000000
            return s)
