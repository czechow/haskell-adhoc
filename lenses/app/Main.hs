module Main where

import Control.Lens

myTp :: (String, String)
myTp = ("Hello", "World")

main :: IO ()
main = do
  putStrLn "Up and running"
  putStrLn $ view _2 myTp
  putStrLn . show $ set _2 "Bailla" myTp
