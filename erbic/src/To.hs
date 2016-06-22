module To where


import System.Timeout
import System.IO
import Control.Concurrent
import Control.Exception
import Network.Socket
import Control.Monad
import GHC.IO.Exception
import qualified Data.Set as S

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
        (\s -> do close s
                  putStrLn "Running handler in bracket")
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
      let t2 = ioe_type e
      putStrLn $ "Exception is [" ++ show t ++ "][" ++ show t2 ++ "][" ++ show e ++ "]"
      return $ Left $ "*** Exception: " ++ show e


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


main :: IO ()
main = do
  errOrSock <- openSock
  case errOrSock of
    Left err -> putStrLn $ "Error opening socket: [" ++ err ++ "]"
    Right s ->
      bracket (do (s', otherAddr) <- accept s
                  putStrLn $ "Conn from [" ++ show otherAddr ++ "] accepted"
                  return s')
              (\s' -> do close s'
                         close s)
              (\s' -> readLoop s')
  where
    readLoop :: Socket -> IO ()
    readLoop s'' = do
      sr <- readSock s''
      case sr of
        SRRClosed -> putStrLn $ "Other party closed channel"
        SRRData str -> do putStrLn $ "Received [" ++ str ++ "]"
                          readLoop s''
        SRRErr Nothing errMsg -> putStrLn $ "Socket error " ++ errMsg
        SRRErr (Just errCode) errMsg ->
          putStrLn $ "Socket error [" ++ show errCode ++ "]: " ++ errMsg

testAsyncExceptions :: IO ()
testAsyncExceptions = do
  nt <- forkIO $ do
    _ <- openSock
    return ()
  threadDelay $ 2 * 1000 * 1000
  putStrLn $ "Killing thread " ++ show nt
  killThread nt
  threadDelay $ 5 * 1000 * 1000
