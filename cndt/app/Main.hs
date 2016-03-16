module Main where

import Data.Conduit
import qualified Data.Conduit.List as CL

main :: IO ()
main = do
  putStrLn "Up and running"

  result <- CL.sourceList [1::Int ..10] $$ CL.fold (+) 0
  print result

  putStrLn "Now pipeline"
  source $$ conduit =$ sink


source :: Source IO Int
source = do
  yield 1
  yield 2

conduit :: Conduit Int IO String
conduit = CL.map $ show . (+3)

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn