{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}



import Prelude hiding (log)

-- import Control.Concurrent
-- import Control.Exception
-- import Network.Socket
-- import Data.Map.Strict as M
-- import Control.Exception
-- import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.Identity
-- import Control.Concurrent.Chan
-- import qualified Data.Text.IO as TIO
-- import System.IO
import Data.IORef

data SSData = SSData { iorName :: IORef String }


mkSSData :: IO SSData
mkSSData = do
  ior <- newIORef "Initialized"
  return $ SSData { iorName = ior }


startAction :: ReaderT SSData IO SSData
startAction = do
  ssd@(SSData { .. }) <- ask
  name <- liftIO $ readIORef iorName
  liftIO $ putStrLn $ "startAction called with state [" ++ name ++ "]"
  liftIO $ modifyIORef iorName $ \_ -> "Started"
  return ssd

stopAction :: ReaderT SSData IO SSData
stopAction = do
  ssd@(SSData { .. }) <- ask
  name <- liftIO $ readIORef iorName
  liftIO $ putStrLn $ "stopAction called with state [" ++ name ++ "]"
  liftIO $ modifyIORef iorName $ \_ -> "Stopped"
  return ssd


startSS :: IO SSData
startSS = do
  ssd <- mkSSData
  ssd' <- runReaderT startAction ssd
  name' <- readIORef (iorName ssd')
  putStrLn $ "startAction changed state to [" ++ name' ++ "]"
  return ssd'


stopSS :: SSData -> IO SSData
stopSS ssd = do
  ssd' <- runReaderT stopAction ssd
  name' <- readIORef (iorName ssd')
  putStrLn $ "stopAction changed state to [" ++ name' ++ "]"
  return ssd'

main :: IO ()
main = do
  putStrLn "Ok"
  ssd <- startSS
  _ <- stopSS ssd
  return ()
