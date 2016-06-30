{-# LANGUAGE LambdaCase #-}

module NTo where


import System.Timeout
import System.IO
import Control.Concurrent
import Control.Exception
import Network.Socket
import Control.Monad
import GHC.IO.Exception
import Data.List.Split
import Data.IORef
import System.Random
import GHC.IO (unsafeUnmask)
import qualified Data.Set as S

type ErrMsg = String

-- This may throw an exception
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


data SockReadRes = SRRData String
                 | SRRClosed
                 | SRRErr (Maybe Int) ErrMsg
                 deriving Show


-- readSock :: Socket -> IO SockReadRes
-- readSock s = readWith (recv s 8)


-- readWith :: IO String -> IO SockReadRes
-- readWith readFun = recv' `catch` handler
--   where
--     recv' = liftM SRRData $ readFun
--     handler :: IOException -> IO SockReadRes
--     handler e = do
--       putStrLn $ "Running e handler"
--       let maybeErrCode = fromInteger . toInteger <$> ioe_errno e
--       putStrLn $ "Exception is [" ++ show maybeErrCode ++ "][" ++ show e ++ "]"
--       case (ioe_type e) of
--         EOF -> return SRRClosed
--         _ -> return $ SRRErr maybeErrCode (show e)

srvConn :: MVar (S.Set ThreadId) -> Socket -> IO ()
srvConn mvThreads s =
  mask $ \restoreMask ->
    do putStrLn $ "srvConn: Accepting requests"
       (s', _) <- accept s
       tid <- forkFinally (thrBody s' restoreMask)
                          (thrHandler s')
       putStrLn $ "srvConn: new connection served by thread " ++ show tid
         where
           thrBody s'' restoreMask'' = do
             modifyMVar_ mvThreads $
                         \ts -> flip S.insert ts <$> myThreadId
             restoreMask'' $ doServeConn s''
           -- FIXME: what if async exception strikes on blocking mvar oper?
           -- probably we should use total masking to avoid the danger...
           thrHandler s'' = \_ -> do
             close s''
             putStrLn $ "Sock closed " ++ show s''
             modifyMVar_ mvThreads $
                         \ts -> flip S.delete ts <$> myThreadId

nset :: IO (MVar (S.Set ThreadId))
nset = newMVar S.empty


doServeConn :: Socket -> IO ()
doServeConn s = do
  ms <- getMaskingState
  putStrLn $ "doServerConn: Mask state is " ++  show ms
  recv s 16 >>= putStrLn
  doServeConn s


doAll :: MVar (S.Set ThreadId) -> Int -> IO ()
doAll mv n = bracket
             (openSock)
             (\s -> do close s
                       putStrLn $ "doAll: Closed socket " ++ show s)
             (\s -> loop n s)
  where
    loop :: Int -> Socket -> IO ()
    loop n' s
      | n' <= 0 = return ()
      | otherwise = do _ <- srvConn mv s
                       loop (pred n') s


mvInt :: IO (MVar Int)
mvInt = newMVar 0


conc :: MVar Int -> IO ()
conc mv = do
  modifyMVar_ mv $ return . const 0
  conc mv


isCloseIntr :: MVar Int -> IO ()
isCloseIntr mv = do
  s <- openSock
  putStrLn $ "Repeatedly closing socket"
  mask_ $ loop s
    where
      loop s' = do close s'
                   uninterruptibleMask $ \_ -> modifyMVar_ mv $ return . succ
                   loop s'

nonIntr :: IO ()
nonIntr = do
  mask_ (do putStrLn "Sleep started"
            threadDelay $ 10 * 1000 * 1000
            putStrLn "Sleep stopped")
