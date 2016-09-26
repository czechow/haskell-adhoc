{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings#-}

module To where


import System.IO
import Control.Concurrent
import Control.Exception
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 (unpack)
import Control.Monad
import GHC.IO.Exception
import Data.List.Split
import Data.IORef
import System.Random
import GHC.IO (unsafeUnmask)

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
                  putStrLn "Running handler in bracket"
                  ms <- getMaskingState
                  putStrLn $ "Mask state is " ++ show ms)
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
      putStrLn $
        "Exception is [" ++ show t ++ "][" ++ show t2 ++ "][" ++ show e ++ "]"
      return $ Left $ "*** Exception: " ++ show e


data SockReadRes = SRRData String
                 | SRRClosed
                 | SRRErr (Maybe Int) ErrMsg
                 deriving Show


readSock :: Socket -> IO SockReadRes
readSock s = readWith $ unpack <$> (recv s 8)


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

scanForMsg :: IO SockReadRes -> Buffer -> IO (Either String ([String], Buffer))
scanForMsg fRead b = readLoop fRead b PPIn
  where
    readLoop fRd buff pp
      | length buff > maxBuffLen = do
          putStrLn $ "Buff too long, dropped " ++
                     show (length buff - maxBuffLen) ++ " chars"
          readLoop fRd (drop (length buff - maxBuffLen) buff) PPOut
      | otherwise = do
          case (splitOn sep buff, pp) of
            (a@(_ : _ : _), PPIn) -> return $ Right $ (init a, last a)
            (a@(m : _ : _), PPOut) -> do
              putStrLn $ "Out of sync [beg], dropped " ++
                         show (length m) ++ "  chars"
              return $ Right $ (init $ tail a, last a)
            (m : ms, PPOut) -> do
              putStrLn $ "Out of sync, dropped " ++ show (length m) ++ " chars"
              readDataAndLoop fRd (concat ms) PPOut
            (_, PPIn) -> do readDataAndLoop fRd buff PPIn
            ([], pp') -> do readDataAndLoop fRd [] pp'

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

sample :: String
sample = "This is a very important text which is very cool"


mockSockReader :: IORef String -> IO SockReadRes
mockSockReader strRef = do
  readIORef strRef >>= \case
    [] -> return SRRClosed
    str -> do
      gen <- newStdGen
      let (i, _) = randomR (0, 8) gen
          (msg, str') = splitAt i str
      writeIORef strRef str'
      return $ SRRData msg

samples :: IO ()
samples = do
  vRef <- newIORef sample
  let f = mockSockReader vRef
  loop f
  where
    loop f' = do
      v <- f'
      case v of
        SRRData msgs -> putStrLn ("[" ++ msgs ++ "]") >> loop f'
        SRRClosed -> putStrLn "*** Closed ***"
        SRRErr _ eMsg -> putStrLn eMsg



testBracket :: Int -> IO ()
testBracket delSecs =
  bracket (openSock)
          (\case Left(err) -> putStrLn err
                 Right(s) -> putStrLn "Closing outer" >>
                             close s)
          (processing)
  where
    processing _ =
      bracket_ (putStrLn "Opening inner")
               (putStrLn "Closing inner")
               (processing')
      where
        processing' = do
          putStrLn $ "Delaying processing thread for " ++ show delSecs ++ "s"
          threadDelay $ delSecs * 1000 * (1000 :: Int)
          putStrLn $ "Calculating fibs"
          let res = fib 30
          putStrLn $ "Res is " ++ show res


tBr :: IO ()
tBr = bracket_ (do ms <- getMaskingState
                   putStrLn $ "A: Mask state is " ++ show ms
                   putStrLn "Fib calc..."
                   allowInterrupt
                   ms' <- getMaskingState
                   putStrLn $ "A: Mask state is " ++ show ms'
                   --threadDelay $ 10 * 1000 * 1000
                   unsafeUnmask $ do
                     ms'' <- getMaskingState
                     putStrLn $ "A: Mask state is " ++ show ms''
                     fibIO 30)
               (do ms <- getMaskingState
                   putStrLn $ "C: Mask state is " ++ show ms)
               (do ms <- getMaskingState
                   putStrLn $ "B: Mask state is " ++ show ms)

fibIO :: Integer -> IO ()
fibIO n = do let x = fib n
             putStrLn $ show x

{-
  Now: check how threads spawned by other thread behave...
-}


testThreads :: IO ()
testThreads = do
  t <- forkIO $ bracket
       (mapM (\_ -> startNewThr) [1::Int .. 3])
       (\thrs -> do putStrLn $ "Killing threads " ++ show thrs
                    mapM_ killThread thrs
                    putStrLn $ "Thread finished")
       (\thrs -> do
           putStrLn $ "Started threads: " ++ show thrs
           threadDelay $ 10 * 1000 * 1000)

  threadDelay $ 3 * 1000 * 1000
  killThread t
  return ()
  where
    startNewThr :: IO ThreadId
    startNewThr = forkFinally
                  (threadDelay $ 10 * 1000 * 1000) $
                  \e -> putStrLn $ "Thread finished by " ++ show e


{-
MainThread
- spawns accThread (bracketed?)
- spawns console reader thread
- waits for both to terminate

Can mainThread be killed??/
-}



accSock :: Socket -> IO (Either ErrMsg Socket)
accSock s =
  mask $ \_ ->
  do
    ms <- getMaskingState
    putStrLn $ "accSock: Mask state is " ++  show ms
    res <- try (do (s', _) <- accept s
                   putStrLn $ "Accept ok, new socket " ++ show s'
                   return s') :: IO (Either SomeException Socket)
    case res of
      Right s'' -> return $ Right s''
      Left ex -> return $ Left $ show ex

srvConn :: Socket -> IO (Either String ThreadId)
srvConn s = mask $ \restoreMask -> do
  accSock s >>= \case
    Left e -> return $ Left e
    Right s' -> do tid <- forkFinally (restoreMask $ doServeConn s')
                          (\_ -> do close s'
                                    putStrLn $ "Sock closed " ++ show s')
                   return $ Right tid

srvConn2 :: Socket -> IO (Either String ThreadId)
srvConn2 s = mask $ \restoreMask -> do
  accSock s >>= \case
    Left e -> return $ Left e
    Right s' -> do r <- try (restoreMask $ doServeConn s')
                   case r of
                     Right _ -> liftM Right myThreadId
                     Left err -> do close s'
                                    putStrLn $ "Sock closed " ++ show s'
                                    return $ Left $ show (err :: SomeException)


doServeConn :: Socket -> IO ()
doServeConn s = do
  ms <- getMaskingState
  putStrLn $ "doServerConn: Mask state is " ++  show ms
  unpack <$> recv s 16 >>= putStrLn
  doServeConn s


doAll :: IO ()
doAll = bracket
        (openSock)
        (\case
            Right s -> do close s
                          putStrLn $ "doAll: Closed socket " ++ show s
            Left e -> putStrLn $ "Error opening socket: " ++ e)
        (\case
            Right s -> (do errOrTid <- srvConn2 s
                           case errOrTid of
                             Right tid -> putStrLn $ "Connection running in thread " ++ show tid
                             Left errMsg -> fail errMsg)

            Left e -> putStrLn e)


accThread :: IO ()
accThread = bracket
            (openSock)
            (\case
                Left err -> putStrLn $ "Error opening socket: [" ++ err ++ "]"
                Right s -> do putStrLn $ "Closing socket " ++ show s
                              close s)
            (\case
                Left _ -> return ()
                Right s -> acceptLoop s)
  where
    acceptLoop s =
      bracketOnError (do (s', otherAddr) <- accept s
                         putStrLn $
                           "Conn from [" ++ show otherAddr ++ "] accepted"
                         putStrLn $ "New socket is " ++ show s'
                         return s')
                     (\s' -> do putStrLn $ "Closing socket " ++ show s'
                                close s')
                     (\s' -> readLoop s')
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


excTest :: IO ()
excTest = (do
  putStrLn "Sleeping";
  threadDelay $ 20 * 1000 * 1000
  putStrLn "Sleep finished") `catch` handler
  where
    handler :: SomeException -> IO ()
    handler e = putStrLn $ "Caught [" ++ show e ++ "]"
