{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

import Data.Typeable
import Data.Data
import Control.Monad.State

import Httpd


data RFQ = RFQ Int String -- and many more...
         deriving (Show, Eq, Typeable, Data)

-- what is a key???
-- do we need generic Rule_Key?
type KeyTemplate = String

data RuleKey a = RK a (Maybe KeyTemplate) String
               | CtrlRK a (Maybe KeyTemplate) String
               deriving Show

makeRuleKey :: Show a => a -> Maybe KeyTemplate -> RuleKey a
makeRuleKey x (Just template) = RK x (Just template) (template ++ show x)
makeRuleKey x Nothing = RK x Nothing (show x)

class Keyable k where
  getKey :: k -> String

instance Keyable (RuleKey a) where
  getKey (RK _ _ key) = key
  getKey (CtrlRK _ _ key) = key

instance Eq (RuleKey a) where
  (==) (RK _ _ k1) (RK _ _ k2) = k1 == k2
  (==) (CtrlRK _ _ k1) (CtrlRK _ _ k2) = k1 == k2
  (==) _ _ = False


-- For S3 (RFQ Circle)
-- we get
--   previous state: [RFQIds]
--   input data: RFQ
-- we output
--   next state : [RFQIds]

-- For S3 (Final)
-- we get
--   previous state: current number
--   input data [RFQIds], Time
-- we output
--   count of 30 secs window


data Value = I Int
           | S String
           deriving Show

data StateValue a where
  V :: a -> StateValue a
  L :: [a] -> StateValue a


-- rule key is a mess because it tries to hide the proper nature of state

type Tick = Int

data Input a = Input a

newtype S1 = S1 [Int] deriving (Show, Eq, Typeable, Data)
newtype S3 = S3 Int deriving (Show, Eq, Typeable, Data)
newtype I1 = I1 RFQ deriving (Show, Eq, Typeable, Data)
newtype T1 = T1 Tick deriving (Show, Eq, Typeable, Data)
newtype O1 = O1 [Int]
newtype O2 = O2 Bool

is1 :: S1
is1 = S1 $ []

is3 :: S3
is3 =  S3 $ 0

f1 :: I1 -> S1 -> (S1, O1)
f1 (I1 (RFQ rfqId _)) (S1 ss) = (S1 $ rfqId : ss, O1 $ rfqId : ss)

f3 :: (O1, T1) -> S3 -> (S3, O2)
f3 (O1 rfqs, T1 _) (S3 _) = (S3 $ length rfqs, O2 $ length rfqs >= 3)

-- This needs monadic state...
-- Entry to the function is the state of all rules plus
-- the event that triggered the computation

-- This may be heterogenous Map, I think.
-- Or a state constructed as a result of DSL application
type AllS = (S1, S3)

-- let's assume we have

-- State monad == StateProcess monad

first :: (a -> a) -> (a, b) -> (a, b)
first f (x, y) = (f x, y)

second :: (b -> b) -> (a, b) -> (a, b)
second f (x, y) = (x, f y)

updateFirst :: S1 -> State AllS ()
updateFirst s1 = do
  allS <- get
  put $ first (const s1) allS

updateSecond :: S3 -> State AllS ()
updateSecond s3 = do
  allS <- get
  put $ second (const s3) allS

fStateFul :: (I1, T1) -> State AllS O2
fStateFul (i1, t1) = do
  (s1, _) <- get
  let (ns1, o1) = f1 i1 s1
  updateFirst ns1
  (_, s3) <- get
  let (ns3, o2) = f3 (o1, t1) s3
  updateSecond ns3
  return o2

fAll :: (I1, T1) -> AllS -> (AllS, O2)
fAll (i1, t1) (ps1, ps3) =
  let (ns1, o1) = f1 i1 ps1
      (ns3, o2) = f3 (o1, t1) ps3
  in ((ns1, ns3), o2)

main :: IO ()
main = do
  putStrLn "Up and running"
  Httpd.run
