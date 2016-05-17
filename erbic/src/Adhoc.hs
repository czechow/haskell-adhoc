module Adhoc where

import qualified Data.OrdPSQ as PQ
import Control.Monad.Writer
import Control.Monad.State

newtype RFQ = RFQ Int deriving (Show, Eq)

instance Ord RFQ where
  (<=) (RFQ x) (RFQ y) = x <= y


q, q2, q3 :: PQ.OrdPSQ RFQ Int String
q = PQ.singleton (RFQ 1) 100 "Inserted first"
--q =

q2 = PQ.insert (RFQ 2) 50 "Second item" q
q3 = PQ.insert (RFQ 3) 25 "Third item" q2

pqDropWhile :: (Ord k, Ord p) => (k -> p -> v -> Bool)
            -> PQ.OrdPSQ k p v
            -> PQ.OrdPSQ k p v
pqDropWhile pred' pq = case PQ.findMin pq of
  Nothing -> pq
  Just (k, p, v) -> if (pred' k p v)
                    then pqDropWhile pred' $ PQ.deleteMin pq
                    else pq

type MyMonad a b c = WriterT a (State b) c


runProc :: Int -> WriterT [Int] (State [String]) Int
runProc _ = do
  st <- get
  put $ "Something here" : st
  tell [997]
  tell [334]
  return 13

go2 :: IO ()
go2 = do
  putStrLn "Monad transformers"
  let ((r, log'), s) = runState (runWriterT $ runProc 13) ["Nothing interesting"]
  putStrLn $ "Result: " ++ show r
  putStrLn $ "Log: " ++ show log'
  putStrLn $ "State: " ++ show s

go :: IO ()
go = do
  putStrLn "Up and running in PSQ"
  putStrLn $ "Priority queue: " ++ show q3
