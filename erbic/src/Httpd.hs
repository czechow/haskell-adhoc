module Httpd where

import Network.Socket
import Control.Concurrent

msg :: String
msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

wt :: Int
wt = 1

run :: IO ()
run = do
  s <- socket AF_INET Stream defaultProtocol
  hostAddr <- inet_addr "127.0.0.1"
  bind s (SockAddrInet 2222 hostAddr)
  listen s 5

  putStrLn "Waiting for connections. Hit <Enter> to stop"
  _ <- forkIO $ accLoop s
  _ <- getLine
  putStrLn "Finishing main thread"
  close s
  threadDelay 1000000

accLoop :: Socket -> IO ()
accLoop s = do
  (s', _) <- accept s
  --putStrLn "Connection accepted"
  _ <- forkIO $ serveConn s'
  accLoop s

serveConn :: Socket -> IO ()
serveConn s = do
  _ <- send s msg
  --putStrLn $ show len ++ " bytes sent"
  close s

-- socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
