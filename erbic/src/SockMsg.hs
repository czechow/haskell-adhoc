module SockMsg where

import Prelude hiding (log)
--import Network.Socket as NS
import Control.Concurrent

import Control.Concurrent
import System.IO
import System.Timeout
import Control.Monad
import Control.Exception

import Network.Socket
import Control.Concurrent
import System.IO
import System.Timeout
import Control.Monad
import Control.Exception
import Data.Char (toUpper)


type Length = Int
data SockResult = Error String
                | Closed
                | Interrupted
                | Data (String, Int)
                deriving (Show)
type ErrMsg = String

recvLen' :: Socket -> Length -> MVar () -> IO SockResult
recvLen' s l mv = do
  errOrData <- try' (timeout 1000000 $ recvLen s l)
  case errOrData of -- can we somehow detect closing condition???
    Left e -> return $ Error $ show e
    Right maybeData -> tryReadMVar mv >>= shouldStop maybeData
  where
    shouldStop _ (Just _) = return $ Interrupted
    shouldStop md Nothing = case md of
      Just d -> return $ Data d
      Nothing -> recvLen' s l mv


-- Error|Warn|Result
data SockMsg = SockMsg String
             | Close


recvMsg :: Socket -> String -> MVar () -> IO (SockMsg, String)
recvMsg s buff mv = do
  let (msg, buff', infos) = readData buff []
  log infos
  case (msg, buff') of
    (Msg m, buff'') -> return (SockMsg m, buff'')
    (MsgPart mp, _) -> doRecv mp
    (NoMsg, _) -> doRecv []
  where
    doRecv mp  = do
      res <- recvLen' s 16 mv
      case res of
        Data (d, _) -> recvMsg s (mp ++ d) mv
        _ -> return (Close, []) -- FIXME: other states too, any buff info?


log :: [String] -> IO ()
log = mapM_ putStrLn

data Msg = Msg String
         | MsgPart String
         | NoMsg
         deriving (Show)

-- FIXME: The following looks like a classical parser's job...
-- FIXME: State & Writer monad here...
readData :: String -> [String] -> (Msg, String, [String])
readData [] infos = (NoMsg, [], infos)
readData buff' infos =
  case break (=='\n') buff' of
    (msg, ('\n' : newBuff')) -> case sane msg of
      Just m -> (Msg m, newBuff', infos)
      Nothing -> readData newBuff'
                 (infos ++ ["Msg too long: " ++ show (length msg)])
    (_, _) -> case sane buff' of
      Just m -> (MsgPart m, [], infos)
      Nothing -> (NoMsg, [],
                  (infos ++ ["Buffer too long: " ++ show (length buff')]))
  where
    sane m' = if length m' <= 20
              then Just m'
              else Nothing


try' :: IO a -> IO (Either SomeException a)
try' op = do
  res <- try op
  case res of
    Left e -> do
      putStrLn $ "***Exception encountered: " ++ show e
      return $ Left e
    _ -> return res






run :: IO ()
run = do
  s <- socket AF_INET Stream defaultProtocol
  setSocketOption s ReuseAddr 1
  hostAddr <- inet_addr "127.0.0.1"
  bind s (SockAddrInet 2222 hostAddr)
  listen s 5

  putStrLn "Waiting for connections. Hit <Enter> to stop"
  stopInfo <- makeStopInfo
  _ <- forkIO $ accLoop s stopInfo
  _ <- getLine
  putStrLn "Signaling accept thread to finish"
  putMVar (stopReq stopInfo) ()
  mapM_ takeMVar [finished stopInfo]
  putStrLn "Accept thread finished"
  close s

data StopInfo = SI { stopReq :: MVar ()
                   , finished :: MVar () }

makeStopInfo :: IO StopInfo
makeStopInfo = do
  mvStopReq <- newEmptyMVar
  mvStopped <- newEmptyMVar
  return $ SI mvStopReq mvStopped

accLoop :: Socket -> StopInfo -> IO ()
accLoop s' stopInfo' = accLoop' s' [] stopInfo'
  where
    accLoop' s connsComm stopInfo = do
      mr <- timeout 1000000 (accept s) -- FIXME: can accept throw an exception?
      case mr of
        Just (s'', _) -> do
          putStrLn "Connection accepted"
          thrStopInfo <- makeStopInfo
          _ <- forkIO $ serveConn s'' thrStopInfo
          accLoop' s (thrStopInfo : connsComm) stopInfo
        Nothing ->
          tryTakeMVar (stopReq stopInfo) >>= stopOrLoop
          where
            stopOrLoop Nothing = do
              connsComm' <- filterM (isEmptyMVar . finished) connsComm
              if (length connsComm' /= length connsComm)
                then putStrLn $ "Dropped " ++
                     show (length connsComm - length connsComm') ++
                     " connections"
                else return ()
              accLoop' s connsComm' stopInfo
            stopOrLoop (Just _) =
              stopAllConns >>
              putMVar (finished stopInfo) ()

            stopAllConns =
              putStrLn ("Stopping " ++ (show $ length connsComm) ++
                        " connections") >>
              mapM_ (flip putMVar () . stopReq) connsComm >>
              mapM_ (takeMVar . finished) connsComm


type Code = Int

data ConnState = ConnClose String
               | ConnLoop
               | ConnMsg (Code, String)
               deriving Show

serveConn :: Socket -> StopInfo -> IO ()
serveConn s (SI mvStopReq mvFinished) = loop []
    where
      loop buff = do
        res <- recvMsg s buff mvStopReq
        case res of
          (SockMsg m, buff') -> putStrLn ("Received msg: [" ++ m ++ "]") >>
                                loop buff'
          (Close, _) -> close s >>
                        putMVar mvFinished ()


--        recvMsg :: Socket -> String -> MVar () -> IO (SockMsg, String)
