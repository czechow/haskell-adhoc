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


type ErrMsg = String

-- This may throw an exception
openSock :: IO Socket
openSock = open'
  where
    open' =
      bracketOnError
        (socket AF_INET Stream defaultProtocol)
        (\s -> do close s
                  ms <- getMaskingState
                  putStrLn $ "openSock: handler in bracket: Mask state is " ++ show ms
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


readSock :: Socket -> IO SockReadRes
readSock s = readWith (recv s 8)


readWith :: IO String -> IO SockReadRes
readWith readFun = recv' `catch` handler
  where
    recv' = liftM SRRData $ readFun
    handler :: IOException -> IO SockReadRes
    handler e = do
      putStrLn $ "Running e handler"
      let maybeErrCode = fromInteger . toInteger <$> ioe_errno e
      putStrLn $ "Exception is [" ++ show maybeErrCode ++ "][" ++ show e ++ "]"
      case (ioe_type e) of
        EOF -> return SRRClosed
        _ -> return $ SRRErr maybeErrCode (show e)

srvConn :: Socket -> IO ThreadId
srvConn s =
  mask $ \restoreMask ->
    do putStrLn $ "srvConn: Accepting requests"
       (s', _) <- accept s
       tid <- forkFinally (restoreMask $ doServeConn s')
              (\_ -> do close s'
                        putStrLn $ "Sock closed " ++ show s')
       putStrLn $ "srvConn: new connection served by thread " ++ show tid
       return tid


doServeConn :: Socket -> IO ()
doServeConn s = do
  ms <- getMaskingState
  putStrLn $ "doServerConn: Mask state is " ++  show ms
  recv s 16 >>= putStrLn
  doServeConn s


doAll :: IO ()
doAll = bracket
        (openSock)
        (\s -> do close s
                  putStrLn $ "doAll: Closed socket " ++ show s)
        (\s -> do _ <- srvConn s
                  return ())
