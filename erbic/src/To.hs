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
      putStrLn $ "Exception is [" ++ show t ++ "][" ++ show e ++ "]"
      return $ Left $ "*** Exception: " ++ show e


data SockReadRes = SRRData String
                 | SRRClose
                 | SRRErr (Maybe Int) ErrMsg
                 deriving Show

closeCodes :: S.Set Int
closeCodes = S.fromList [104]


readSock :: Socket -> IO SockReadRes
readSock s = recv' `catch` handler
  where
    recv' = liftM SRRData $ recv s 8
    handler :: IOException -> IO SockReadRes
    handler e = do
      putStrLn $ "Running e handler"
      let maybeErrCode = fromInteger . toInteger <$> ioe_errno e
      putStrLn $ "Exception is [" ++ show maybeErrCode ++ "][" ++ show e ++ "]"
      case maybeErrCode of
        Nothing -> return $ SRRErr Nothing (show e)
        Just code -> if S.member code closeCodes
                     then return SRRClose
                     else return $ SRRErr (Just code) (show e)
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
