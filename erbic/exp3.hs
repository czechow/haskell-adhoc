{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}


import Prelude hiding (log)

-- import Control.Concurrent
-- import Control.Exception
-- import Network.Socket
-- import Data.Map.Strict as M
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Concurrent.Chan
import qualified Data.Text.IO as TIO
import System.IO



class SockService ss where
  ssRead :: ss -> String

instance SockService Int where
  ssRead x = "Text from sock service " ++ show x

instance SockService String where
  ssRead x = "Text here: " ++ x


type MyApp a = forall ss. SockService ss => ss -> Reader String a



startSvc :: forall ss. SockService ss => ss -> Reader String [String]
startSvc ss = do
  st <- ask
  return $ ["res", st, ssRead ss]


startApp :: [String]
startApp = runReader (startSvc (56 :: Int)) ("State")

startTest :: [String]
startTest = runReader (startSvc "Test") ("State")

main :: IO ()
main = do
  putStrLn $ "App:  [" ++ show startApp ++ "]"
  putStrLn $ "Test: [" ++ show startTest ++ "]"


-- Rank2Type
foo :: (forall a. (a -> a)) -> (Bool, Char)
foo f = (f True, f 'a')
