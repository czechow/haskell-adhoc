{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Adhoc where

import Data.List (intersperse, intercalate)
import qualified Data.Map as M


data Out = Out [String]
         deriving (Show)

data Rd = Rd String
        deriving (Show)

data Kdb = Kdb String
         deriving (Show)

data File = File String
          deriving (Show)

mread :: Int -> (Int -> Rd) -> Out
mread cnt rdFn =
  let res = rdFn cnt
      transformFn (Rd s) = Out $ replicate 2 $ reverse s
  in transformFn res

kdbRead :: Kdb -> Int -> Rd
kdbRead (Kdb xs) =
  let _ =  length xs
  in \cnt -> Rd $ concat $ replicate cnt $ intersperse '-' "Wania"

fileRead :: File -> Int -> Rd
fileRead (File _) =
 \cnt -> Rd $ concat $ replicate cnt $ "Tom"


type Name = String
data Type = Tps15mSW
          | TpsDl
          | TallDl
          deriving Show

type Ecn = String
type SecurityID = String
type Side = String

data NCBKey a = NCBKey a

instance Show (NCBKey (SecurityID, Side, Ecn)) where
  show (NCBKey (securityID, side, ecn)) =
    intercalate "-" [show securityID, show side, show ecn]


class Keyable a b | a -> b where
  getKey :: a -> b
  toStr :: a -> String

instance Show a => Keyable (NCBKey a) a where
  getKey (NCBKey k') = k'
  toStr = show . getKey


data NCBps15mSW = NCBps15mSW (NCBKey (SecurityID, Side, Ecn)) Int
                deriving Show
data NCBpsDl = NCBpsDl SecurityID Int
             deriving Show
data NCBallDl = NCBallDl Int
              deriving Show


class CB a b | a -> b where
  tpe :: a -> Type
  key :: a -> b
  name :: a -> String

instance CB NCBps15mSW (SecurityID, Side, Ecn) where
  tpe _ = Tps15mSW
  name cb@(NCBps15mSW k' _) =
    intercalate "-" [show $ tpe cb, toStr k']
  key (NCBps15mSW (NCBKey k') _) = k'

instance CB NCBallDl () where
  tpe _ = TallDl
  name cb = show $ tpe cb
  key _ = ()

procFoo :: (CB a b) => [a] -> [String]
procFoo xs = map name xs

class MShow a where
  toMap :: a -> M.Map String String

class MRead a where
  fromMap :: M.Map String String -> a


-- instance CB CBps15mSW where
--   tpe (CBps15mSW cb) = tp cb
--   key (CBps15mSW cb) = case k cb of
--     NCBKey k' -> Just $ show k'
--     NCBKeyEmpty -> Nothing
