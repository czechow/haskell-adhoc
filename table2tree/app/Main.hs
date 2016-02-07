module Main where

import qualified Data.Tree as T

import Lib

main :: IO ()
main = putStrLn "Up and running" >>
       putStrLn (T.drawTree myTree) >>
       putStrLn "Finished"
