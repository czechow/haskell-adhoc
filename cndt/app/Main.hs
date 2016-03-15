module Main where

import Data.Conduit
import qualified Data.Conduit.List as CL

main :: IO ()
main = do
  putStrLn "Up and running"

  result <- CL.sourceList [1::Int ..10] $$ CL.fold (+) 0
  print result
