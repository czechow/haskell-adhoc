module Adhoc where

import qualified Data.OrdPSQ as PQ

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

go :: IO ()
go = do
  putStrLn "Up and running in PSQ"
  putStrLn $ "Priority queue: " ++ show q3
