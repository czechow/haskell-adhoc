module Main where

import Control.Concurrent
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import Lib



main :: IO ()
main = do
  putStrLn "Up and running"
  Right t <- createTransport "127.0.0.1" "11235" defaultTCPParameters
  node <- newLocalNode t initRemoteTable

  _ <- runProcess node $ do
    self <- getSelfPid
    send self "Hello"
    hello <- expect :: Process String
    liftIO $ putStrLn $ "I received " ++ hello

  putStrLn "Finished"
