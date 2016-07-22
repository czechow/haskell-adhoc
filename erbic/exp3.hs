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

newtype Dollars a = Dollars a deriving (Eq, Show, Num)

-- s will be datatype supporting logger
class Logger s where
  lg :: s -> String -> IO ()


instance Logger (Chan String) where
  lg ch s = writeChan ch s



type App r a = (ReaderT r IO a)



runApp :: App r a -> r -> IO a
runApp app' x = runReaderT app' x


-- ReaderT r m a
app :: Logger a => App a Int
app = do
  ch <- ask
  liftIO $ lg ch "Wania"
  return 13


-- safeLoadFile :: FilePath -> IO (Either IOException String)
-- safeLoadFile fp = (Right <$> readFile fp) `catch` \e -> return $ Left e

main :: IO ()
main = do
  putStrLn $ "Up and running"
--   x <- liftM (fmap length) (safeLoadFile "test.txt")
  ch <- newChan
  res <- runApp (app :: App (Chan String) Int) ch
  chres <- readChan ch
  putStrLn $ show res
  putStrLn $ "Read from ch: [" ++ chres ++ "]"
  return ()
