{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SrvSpike where


--import GHC.Generics
import Servant -- perhaphs qualified
--import Servant.Server.Experimental.Auth
--import Data.Aeson (ToJSON, FromJSON)
import qualified Network.Wai.Handler.Warp as Warp
--import qualified Network.Wai.Handler.WarpTLS as Warp
--import Network.Wai (Request, requestHeaders)
import Control.Monad.Except
--import Data.Text (Text)
import Data.IORef
--import qualified Data.Map.Strict as M
--import Control.Concurrent (threadDelay)
--import Data.List.Split (splitOn)
--import Data.Maybe (catMaybes)
--import Data.ByteString.Char8 (unpack)

import Control.Monad.Reader

type MyReaderT = ReaderT (IORef Int) IO


readerToHandler :: IORef Int -> MyReaderT :~> Handler
readerToHandler ref = Nat $ readerToHandler' ref

readerToHandler' :: IORef Int -> MyReaderT a -> Handler a
readerToHandler' ref r = do
  liftIO $ runReaderT r ref

type ReaderAPI = "a" :> Get '[JSON] Int
            :<|> "b" :> Get '[JSON] String

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy


readerServerT :: ServerT ReaderAPI MyReaderT
readerServerT = a :<|> b
  where a :: MyReaderT Int
        a = return 42

b :: MyReaderT String
b = do
  ref <- ask
  liftIO $ putStrLn $ "Here "
  y <- liftIO $ atomicModifyIORef' ref (\x -> (succ x, succ x))
  return $ "Current identifier: " ++ show y


readerServer :: IORef Int -> Server ReaderAPI
readerServer ref = do
  enter (readerToHandler ref) readerServerT

app4 :: IORef Int -> Application
app4 ref =
  serve readerAPI (readerServer ref)

go :: IO ()
go = do
  ref <- newIORef 0
  Warp.run 2222 $ app4 ref


data Conf = Conf { count :: IORef Int
                 , other :: IORef String
                 }

mkConf :: IO Conf
mkConf = do
  cntRef <- newIORef 0
  strRef <- newIORef "test me"
  return $ Conf cntRef strRef

showConf :: Conf -> IO ()
showConf (Conf cntRef otherRef) = do
  cnt <- readIORef cntRef
  oth <- readIORef otherRef
  putStrLn $ "Result: " ++ show cnt ++ ", " ++ oth


type MyConfReader = ReaderT Conf IO

modifyConf :: MyConfReader ()
modifyConf = do
  cnf@(Conf cRef _) <- ask
  liftIO $ putStrLn "Conf before modification"
  liftIO $ showConf cnf
  liftIO $ modifyIORef' cRef (+1)
  liftIO $ putStrLn "Conf after modification"
  liftIO $ showConf cnf
  return ()
