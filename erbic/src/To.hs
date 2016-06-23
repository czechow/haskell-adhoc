module To where


import System.Timeout
import System.IO
import Control.Concurrent
import Control.Exception
import Network.Socket
import Control.Monad
import GHC.IO.Exception
import Data.List.Split

longFileRead :: Handle -> IO String
longFileRead h = do
  threadDelay 1000000
  line1 <- hGetLine h
  threadDelay 1000000
  line2 <- hGetLine h
  return $ line1 ++ line2


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
  -- FIXME: possibly race conditions here =>
  -- We already have a socket, but if an async exception happens here
  -- we will be loosing the socket...
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

-- A function "read from socket" that delivers full messages
-- IO SocketReadRes

{-
  loop:
    scanForPackets
      socket <- readFromSocket(Err,Msg,Nothing)
      case closed channel => issue msg, close socket, kill itself
      case Err => report error, close socket, kill itself
      case Data => put data into channel; loop
-}


sep :: String
sep = "-"

maxBuffLen :: Int
maxBuffLen = 13

data PacketParse = PPIn | PPOut

type Buffer = String

-- FIXME: bracket here?
scanForMsg :: IO SockReadRes -> Buffer -> IO (Either String ([String], Buffer))
scanForMsg fRead b = readLoop fRead b PPIn
  where
    readLoop :: IO SockReadRes -> Buffer -> PacketParse
             -> IO (Either String ([String], Buffer))
    readLoop fRd buff pp
      | length buff > maxBuffLen = do
          putStrLn $ "Buff too long, dropped " ++ show (length buff - maxBuffLen) ++ " chars"
          readLoop fRd (drop (length buff - maxBuffLen) buff) PPOut
      | otherwise = do
          case (splitOn sep buff, pp) of
            (a@(_ : _ : _), PPIn) -> return $ Right $ (init a, last a)
            (a@(m : _ : _), PPOut) -> do
              putStrLn $ "Out of sync [beg], dropped " ++ show (length m) ++ "  chars"
              return $ Right $ (init $ tail a, last a)
            (m : ms, PPOut) -> do
              putStrLn $ "Out of sync, dropped " ++ show (length m) ++ " chars"
              readDataAndLoop fRd (concat ms) PPOut
            (_, PPIn) -> do readDataAndLoop fRd buff PPIn
            ([], pp') -> do readDataAndLoop fRd [] pp'

    readDataAndLoop :: IO SockReadRes -> Buffer -> PacketParse
                    -> IO (Either String ([String], Buffer))
    readDataAndLoop fRd buff pp = do
      res <- fRd
      case res of
        SRRClosed -> return $ Left "All ok, channel closed"
        SRRErr code _ -> return $ Left $ "Some error: " ++ show code
        SRRData d -> readLoop fRd (buff ++ d) pp




fib :: Integer -> Integer
fib n
  | n <= 0 = 0
  | n == 1 = 1
  | n == 2 = 2
  | otherwise = fib (n - 2) + fib (n - 1)

fthr :: IO ThreadId
fthr = forkIO $ finally (do
                            putStrLn "Start"
                            let x = fib 36
                            putStrLn "Stop"
                            putStrLn ""
                            putStrLn $ "Fib is: " ++ show x
                            putStrLn "-------------------------------------------------"
                        )
                        (putStrLn "Exception, cleanup")


run :: IO ()
run = do
  s <- openSock
  case s of
    Left(err) -> putStrLn err
    Right(s0) -> do
      (s', addr) <- accept s0
      putStrLn $ "Conn from " ++ show addr
      loopForMessages s' []
      close s0
  where
    loopForMessages s' buff = do
      errOrMsgs <- scanForMsg (readSock s') buff
      case errOrMsgs of
        Right (msgs, buff') -> do
          mapM_ (\m -> putStrLn $ "Received msg [" ++ m ++ "]") msgs
          loopForMessages s' buff'
        Left err -> do
          putStrLn err
          putStrLn "Stopping program"
          close s'
