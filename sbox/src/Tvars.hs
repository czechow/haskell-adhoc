module Tvars where

import Control.Concurrent.STM.TVar
import Control.Monad.STM

go :: IO ()
go = do
  tv <- newTVarIO (13 :: Int)

  b <- readTVarIO tv
  ret <- atomically $ change tv

  putStrLn $ "Before: " ++ show b
  putStrLn $ "After: " ++ show ret

  return ()

change :: TVar Int -> STM Int
change tv1 = do
  x <- readTVar tv1
  writeTVar tv1 $ succ x
  y <- readTVar tv1
  return y
