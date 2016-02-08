{-# LANGUAGE TemplateHaskell #-}

module LibTest where

import System.Random

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Lib

test_shuffleStrategies :: [TestTree]
test_shuffleStrategies =
  [ QC.testProperty "shuffle_slow and shuffle work the same" $
    \list g -> shuffle_slow (mkStdGen g) (list :: [Int])
               == Lib.shuffle (mkStdGen g) list
  ]

test_shuffle :: [TestTree]
test_shuffle =
  [ testCase "My name should be identical" $
    "czechow" `compare` "czechow" @?= EQ
  ]

tests :: TestTree
tests = $(testGroupGenerator)
