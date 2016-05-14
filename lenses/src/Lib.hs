{-# LANGUAGE TemplateHaskell #-}

module Lib where


import Control.Monad.State
import Control.Lens
import Control.Lens.Zoom

data S = S { _s1 :: Int, _s2 :: Int } deriving Show
newtype Result = Result Int deriving Show

makeLenses ''S


-- This is really a computation
res13 :: State S Result
res13 = state $ \s-> (Result 13, s)


outputPlusOne :: Int -> State S Result
outputPlusOne x = state $ \s -> (Result $ succ x, s)

oper :: Int -> State S Result
oper _ = do
  st <- get
  let res = succ $ view s1 st
  state $ \s -> (Result res, s)
