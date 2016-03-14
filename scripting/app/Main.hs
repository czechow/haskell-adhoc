module Main where

import Control.Monad.IO.Class
import Data.Conduit.Shell
--import System.Directory

main :: IO ()
main = do
  putStrLn "Up and running"
  run (do
          res <- (ls "-al" "." $| grep "stack" $| myFun $| cat)
          liftIO $ putStrLn "Wania"

          liftIO $ putStrLn $ show res
      )

myFun = undefined
