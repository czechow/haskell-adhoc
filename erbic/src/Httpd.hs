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
  mr <- timeout 1000000 (accept s)
  case mr of
    Just (s', _) -> do
      putStrLn "Connection accepted"
      _ <- forkIO $ serveConn s'
      accLoop s
    Nothing -> do
      putStrLn "Connection accept timed out, retrying"
      accLoop s

  -- (s', _) <- accept s
  -- --putStrLn "Connection accepted"
  -- _ <- forkIO $ serveConn s'
  -- accLoop s

serveConn :: Socket -> IO ()
serveConn s = do
  _ <- send s msg
  --putStrLn $ show len ++ " bytes sent"
  close s

-- socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
