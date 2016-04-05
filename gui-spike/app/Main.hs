module Main where

import qualified Invoice as I

main :: IO ()
main = do
  putStrLn "Up and running"
  let cust = I.CustomerInfo 12 "NAME"
  putStrLn $ show cust
