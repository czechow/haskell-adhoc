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
import Control.Monad.Identity
import Control.Monad.Writer
-- import Control.Monad.Identity
import Control.Concurrent.Chan
-- import qualified Data.Text.IO as TIO
import System.Timeout
import Data.IORef


data SSData l = SSData { iorName :: IORef String, logger :: l }

class Logger l where
  lWrite :: l -> String -> IO ()
  lRead :: l -> IO String


mkSSData :: Logger l => l -> IO (SSData l)
mkSSData l = do
  ior <- newIORef "Initialized"
  return $ SSData { iorName = ior, logger = l }


startAction :: Logger l => ReaderT (SSData l) IO (SSData l)
startAction = do
  ssd@(SSData { .. }) <- ask
  name <- liftIO $ readIORef iorName
  liftIO $ do lWrite logger $ "startAction called with state [" ++ name ++ "]"
              modifyIORef iorName $ \_ -> "Started"
  return ssd

stopAction :: Logger l => ReaderT (SSData l) IO (SSData l)
stopAction = do
  ssd@(SSData { .. }) <- ask
  name <- liftIO $ readIORef iorName
  liftIO $ do lWrite logger $ "stopAction called with state [" ++ name ++ "]"
              modifyIORef iorName $ \_ -> "Stopped"
  return ssd


startSS :: Logger l => l -> IO (SSData l)
startSS l = do
  ssd <- mkSSData l
  ssd' <- runReaderT startAction ssd
  name' <- readIORef (iorName ssd')
  lWrite l $ "startAction changed state to [" ++ name' ++ "]"
  return ssd'


stopSS :: Logger l => SSData l -> IO (SSData l)
stopSS ssd = do
  ssd' <- runReaderT stopAction ssd
  name' <- readIORef (iorName ssd')
  lWrite (logger ssd) $ "stopAction changed state to [" ++ name' ++ "]"
  return ssd'

main :: IO ()
main = do
  putStrLn "Ok"
  --ch <- newChan :: IO (Chan String)
  l <- mkTL
  ssd <- startSS l
  _ <- stopSS ssd

  readLog l
  return ()
  where
    readLog :: Logger l => l -> IO ()
    readLog ch = do
      mv <- timeout 1000 $ lRead ch
      case mv of
        Just x -> do putStrLn $ "[LOG]: " ++ x
                     readLog ch
        Nothing -> return ()

instance Logger (Chan String) where
  lWrite ch msg = writeChan ch msg
  lRead ch = readChan ch

data ThrottlingLogger = TL { iorCnt :: IORef Int, logger' :: Chan String }

mkTL :: IO ThrottlingLogger
mkTL = do
  ic <- newIORef 0
  l <- newChan
  return $ TL ic l


instance Logger ThrottlingLogger where
  lWrite TL {..} msg = do
    cnt <- readIORef iorCnt
    if cnt >= 2
      then putStrLn "Message limit exceeded"
      else do modifyIORef' iorCnt (+1)
              writeChan logger' msg
  lRead TL {..} = readChan logger'
