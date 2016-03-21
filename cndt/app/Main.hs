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

  putStrLn "Now pipeline2"
  source $$ conduit2 =$ sink


source :: Source IO Int
source = do
  yield 1
  yield 2

conduit :: Conduit Int IO String
conduit = CL.map $ show . (+3)

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

conduit2 :: Conduit Int IO String
conduit2 = do
  mi <- await
  case mi of
    Just i -> do
      yield $ show i
      conduit2
    Nothing -> return ()


-- Takes in a value (if there is no value, what should we do?)
awFor :: Monad m => (i -> ConduitM i o m r) -> ConduitM i o m ()
awFor f = do
  mi <- await
  case mi of
    Just i -> do
      _ <- f i
      awFor f
    Nothing -> return ()
