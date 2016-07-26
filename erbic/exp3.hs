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

startSvc :: MyApp [String]
startSvc ss = do
  st <- ask
  return $ ["res", st, ssRead ss]



main :: IO ()
main = do
  let x = runReader (startSvc (56 :: Int)) ("Wania")
  putStrLn $ "End: [" ++ show x ++ "]"
