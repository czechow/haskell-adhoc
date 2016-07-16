{-# LANGUAGE LambdaCase #-}

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
  mv <- newEmptyMVar :: IO (MVar Bool)
  tid <- forkFinally action (putMVar mv ())
  return (tid, mv)


socketAcc = do
  bracket (do s <- accept undefined
              (tid, mv) <- ffork $ undefined s -- ffork cannot throw nor block
              return (s, tid, mv))
          (\(s, tid, mv) -> do close s -- this can probably block..
                               killThread tid
                               takeMVar mv) -- this can block...
