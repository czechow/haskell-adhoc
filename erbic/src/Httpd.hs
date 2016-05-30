module Httpd where

import Network.Socket
import Control.Concurrent
import System.IO
import System.Timeout

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

-- FIXME: No filtering of finished threads so far
accLoop :: Socket -> [(MVar (), MVar ())] -> MVar (MVar ()) -> IO ()
accLoop s connsComm mvStopReq = do
  mr <- timeout 1000000 (accept s)
  case mr of
    Just (s', _) -> do
      putStrLn "Connection accepted"
      mvThrStopReq <- newEmptyMVar
      mvThrStopped <- newEmptyMVar
      _ <- forkIO $ serveConn s' mvThrStopReq mvThrStopped
      accLoop s ((mvThrStopReq, mvThrStopped) : connsComm) mvStopReq
    Nothing ->
      tryTakeMVar mvStopReq >>= stopOrLoop []
  where
    stopOrLoop _ Nothing = accLoop s connsComm mvStopReq
    stopOrLoop _ (Just stopReq) =
      stopAllConns >>
      putMVar stopReq ()
    stopAllConns =
      putStrLn ("Stopping " ++ (show $ length connsComm) ++ " connections") >>
      mapM_ (\(mvStopRq, _) -> putMVar mvStopRq ()) connsComm >>
      mapM_ (\(_, mvFinished) -> takeMVar mvFinished) connsComm




serveConn :: Socket -> MVar () -> MVar () -> IO ()
serveConn s mvStopReq mvFinished = do
  _ <- send s "100: HELLO\n"
  loop
    where
      loop = timeout 1000000 (recvLen s 1024) >>= processOrTerminate
      processOrTerminate maybeMsg =
        tryTakeMVar mvStopReq >>= \stopReq -> case stopReq of
          Nothing -> case maybeMsg of
            Just msg' -> return msg' >>= sanitize >>= dispatch
            Nothing -> loop -- timed out, ok to loop again
          Just _ ->
            putStrLn "Thread termination request received" >>
            finish
      sanitize a@(_, 0) = return a
      sanitize a@(input, ln) = case reverse input of
        ('\n' : '\r' : xs) -> return (reverse xs, ln)
        _ -> return a
      dispatch (_, 0) =
        putStrLn "Other party closed connection" >>
        finish
      dispatch ("CLOSE", _) =
        putStrLn "Close command received, closing channel" >>
        finish
      dispatch (cmd, _) =
        send s ("500: Unknown command received [" ++ cmd ++ "]\r\n") >>
        loop
      finish =
        shutdown s ShutdownBoth >>
        close s >>
        putMVar mvFinished ()
