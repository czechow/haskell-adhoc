module Httpd where

import Network.Socket
import Control.Concurrent

msg :: String
msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"


run :: IO ()
run = do
  s <- socket AF_INET Stream defaultProtocol
  hostAddr <- inet_addr "127.0.0.1"
  bind s (SockAddrInet 2222 hostAddr)
  listen s 5

  putStrLn "Waiting for connections"
  (s', _) <- accept s
  putStrLn "Connection accepted"

  len <- send s' msg

  putStrLn $ show len ++ " bytes sent"
  threadDelay 1000000


  close s'
  close s
  return ()

-- socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
