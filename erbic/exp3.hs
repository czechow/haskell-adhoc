{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (log)

-- import Control.Concurrent
-- import Control.Exception
-- import Network.Socket
-- import Data.Map.Strict as M
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.Chan
import qualified Data.Text.IO as TIO
import System.IO
import Data.Text


newtype HandleLoggerT m a = HandleLoggerT (ReaderT Handle m a)


class Monad m => MonadLogger m where
    askLogger :: m (Text -> IO ())

--instance Monad m => MonadLogger (ReaderT Handle m) where
instance Monad m => MonadLogger (HandleLoggerT m) where
  askLogger = do
    handle <- HandleLoggerT ask
    return $ TIO.hPutStrLn handle


-- Monad m =>
-- MonadReader r m =>

myApp :: String -> String
myApp = undefined

-- -- s will be datatype supporting logger
-- class Monad s =>  MonadLogger s where
--   lg :: s (String -> IO ())


-- instance MonadLogger s where
--   lg = undefined


-- type App r a = ReaderT r IO a


-- runApp :: App r a -> r -> IO a
-- runApp app' x = runReaderT app' x


-- -- ReaderT r m a
-- app :: MonadLogger a => App a Int
-- app = do
--   ch <- ask
--   liftIO $ lg ch "Wania"
--   return 13


-- main :: IO ()
-- main = do
--   putStrLn $ "Up and running"
--   ch <- newChan
--   res <- runApp (app :: App (Chan String) Int) ch
--   chres <- readChan ch
--   putStrLn $ show res
--   putStrLn $ "Read from ch: [" ++ chres ++ "]"
--   return ()
