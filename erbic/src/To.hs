module To where


import System.Timeout
import System.IO
import Control.Concurrent
import Control.Exception
import Network.Socket
import Control.Monad
import GHC.IO.Exception

longFileRead :: Handle -> IO String
longFileRead h = do
  threadDelay 1000000
  line1 <- hGetLine h
  threadDelay 1000000
  line2 <- hGetLine h
  return $ line1 ++ line2

run :: IO ()
run = do
  h <- openFile "test.txt" ReadMode
  mto <- timeout 1500000 (longFileRead h)
  case mto of
    Just c -> putStrLn $ "Long read cont: [" ++ c ++ "]"
    Nothing -> putStrLn "Timed out"
  hGetContents h >>= \r -> (putStrLn $ "Rest: [" ++ r ++ "]")
  hClose h

type ErrMsg = String

openSock :: IO (Either ErrMsg Socket)
openSock = open `catch` handler
  where
    open =
      bracketOnError
        (socket AF_INET Stream defaultProtocol)
        (close)
        (\s -> do
            setSocketOption s ReuseAddr 1
            hostAddr <- inet_addr "127.0.0.1"
            putStrLn $ "Acquired socket " ++ show s
            bind s (SockAddrInet 2222 hostAddr)
            listen s 5
            threadDelay 5000000
            return $ Right s)
    handler :: IOException -> IO (Either ErrMsg Socket)
    handler e = do
      putStrLn $ "Running e handler"
      let t = ioe_errno e
      putStrLn $ "Exception is [" ++ show t ++ "][" ++ show e ++ "]"
      return $ Left $ "*** Exception: " ++ show e


readSock :: Socket -> IO (Either ErrMsg String)
readSock s = recv' `catch` handler
  where
    recv' = liftM Right $ recv s 8
    handler :: IOException -> IO (Either ErrMsg String)
    handler e = do
      putStrLn $ "Running e handler"
      let t = ioe_errno e
      putStrLn $ "Exception is [" ++ show t ++ "][" ++ show e ++ "]"
      return $ Left $ "*** Exception: " ++ show e

{-
  readFromSocket chunk => if it fails we should be closing the socket

  What we really want to have is an event driven logic processing
  triggering loop separated from other processing with a bounded queue

  s <- readFromSocket -- wrapped with either => no exceptions

-}

testAsyncExceptions :: IO ()
testAsyncExceptions = do
  nt <- forkIO $ do
    _ <- openSock
    return ()
  threadDelay $ 2 * 1000 * 1000
  putStrLn $ "Killing thread " ++ show nt
  killThread nt
  threadDelay $ 5 * 1000 * 1000
