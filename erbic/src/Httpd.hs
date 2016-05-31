module Httpd where

import Network.Socket
import Control.Concurrent
import System.IO
import System.Timeout
import Control.Monad
import Control.Exception
import Data.Char (toUpper)

msg :: String
msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

wt :: Int
wt = 1

data MyData = MD1 { _id :: Int
                  , _name :: String
                  }
            | MD2 { _time :: Integer }
            deriving (Show, Read)

initMD :: [MyData]
initMD = [ MD1 13 "years"
         , MD1 997 "Police"
         , MD2 1234
         ]

myRead :: IO ()
myRead = do
  h <- openFile "input.txt" ReadMode
  cnt <- rd (0 :: Int) h
  putStrLn $ "Lines " ++ show cnt
  hClose h
  return ()
    where
      rd i h' = do
        eof <- hIsEOF h'
        if eof
          then return i
          else do
            line <- hGetLine h'
            rd (i + length (read line :: [MyData])) h'


run :: IO ()
run = do
  s <- socket AF_INET Stream defaultProtocol
  setSocketOption s ReuseAddr 1
  hostAddr <- inet_addr "127.0.0.1"
  bind s (SockAddrInet 2222 hostAddr)
  listen s 5

  putStrLn "Waiting for connections. Hit <Enter> to stop"
  mvStopReq <- newEmptyMVar
  _ <- forkIO $ accLoop s [] mvStopReq
  _ <- getLine
  putStrLn "Signaling accept thread to finish"
  stopReqCallback <- newEmptyMVar
  putMVar mvStopReq stopReqCallback
  mapM_ takeMVar [stopReqCallback]
  putStrLn "Accept thread finished"
  shutdown s ShutdownBoth
  close s

data StopInfo = SI { stopReq :: MVar ()
                   , finished :: MVar () }

accLoop :: Socket -> [StopInfo] -> MVar (MVar ()) -> IO ()
accLoop s connsComm mvStopReq = do
  mr <- timeout 1000000 (accept s)
  case mr of
    Just (s', _) -> do
      putStrLn "Connection accepted"
      mvThrStopReq <- newEmptyMVar
      mvThrStopped <- newEmptyMVar
      _ <- forkIO $ serveConn s' $ SI mvThrStopReq mvThrStopped
      accLoop s (SI mvThrStopReq mvThrStopped : connsComm) mvStopReq
    Nothing ->
      tryTakeMVar mvStopReq >>= stopOrLoop []
  where
    stopOrLoop _ Nothing = do
      connsComm' <- filterM (isEmptyMVar . finished) connsComm
      if (length connsComm' /= length connsComm)
        then putStrLn $ "Dropped " ++
                        show (length connsComm - length connsComm') ++
                        " connections"
        else return ()
      accLoop s connsComm' mvStopReq
    stopOrLoop _ (Just stopReq') =
      stopAllConns >>
      putMVar stopReq' ()
    stopAllConns =
      putStrLn ("Stopping " ++ (show $ length connsComm) ++ " connections") >>
      mapM_ (flip putMVar () . stopReq) connsComm >>
      mapM_ (takeMVar . finished) connsComm



-- FIXME: send may fail as well
-- FIXME: reading until newline?
serveConn :: Socket -> StopInfo -> IO ()
serveConn s (SI mvStopReq mvFinished) = do
  _ <- send s "100: HELLO\n"
  loop
    where
      loop = read' >>= shouldTerminate >>= process >>= loopOrStop

      read' :: IO (Either SomeException (Maybe (String, Int)))
      read' =  try (timeout 1000000 $ recvLen s 1024)

      shouldTerminate (Left _) =
        return $ Left "Other party closed connection"
      shouldTerminate (Right x) = do
       stopReq' <- tryTakeMVar mvStopReq
       case stopReq' of
         Just _ -> return $ Left "Thread termination request received"
         Nothing -> return $ Right x

      process (Left x) = return $ Left x
      process (Right (Just (msg', _))) = return $ processMsg $ map toUpper msg'
      process (Right Nothing) = return $ Right Nothing

      loopOrStop (Right (Just (code, msg'))) =
        send s (show code ++ ": " ++ msg' ++ "\r\n") >>
        loop
      loopOrStop (Right Nothing) = loop
      loopOrStop (Left errMsg) = putStrLn errMsg >>
                                 shutdown s ShutdownBoth >>
                                 close s >>
                                 putMVar mvFinished ()

type Code = Int

processMsg :: String -> Either String (Maybe (Code, String))
processMsg =  dispatch . sanitize
  where
    sanitize input = case reverse input of
      ('\n' : '\r' : xs) -> reverse xs
      _ -> input
    dispatch "CLOSE" = Left "Close command received, closing channel"
    dispatch cmd =
      Right $ Just (500, "Unknown command received [" ++ cmd ++ "]")
