{-# LANGUAGE TemplateHaskell #-}

module LibTest where

import System.Random
import Data.List

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


-- FIXME: Any mathematical base for checking?
-- convert this to more generic type
test_shuffle :: [TestTree]
test_shuffle =
  [ QC.testProperty "distribution after shuffling" $
    QC.once $ \g -> checkShuffle (mkStdGen g) [1..10]
  ]
  where
    checkShuffle :: StdGen -> [Int] -> Bool
    checkShuffle _ [] = True
    checkShuffle g xs = (var / fromIntegral avg) < (0.05 :: Double)
      where
        n = 1000
        gens = take n $ iterate (snd . next) g
        xxs = map (flip Lib.shuffle xs) gens
        sums = map sum $ Data.List.transpose xxs
        avg = sum sums `quot` length sums
        diffs = map (subtract avg) sums
        sumsRo2 = sum $ map (^(2 :: Int)) diffs
        var = sqrt $ fromIntegral $ sumsRo2 `quot` length sums

-- now - test forest???

test_queryDeps :: [TestTree]
test_queryDeps =
  [ testCase "query empty table" $
    [] @=? queryDeps (buildDeps []) "Anything"
  , testCase "query nonexistent element" $
    [] @=? queryDeps m "Non-Existent"
  , testCase "query one element" $
    [("RA", "")] @=? queryDeps m "RA"
  , testCase "query multiple elements" $
    [("R", "RA"), ("RA", "")] @=? queryDeps m "R"
  , testCase "query table with cycles" $
    [("R", "RA"), ("RA", "R")] @=? queryDeps m' "R"
  ]
  where
    m = buildDeps [("R", "RA"), ("RA", ""), ("O", "OA")]
    m' = buildDeps [("R", "RA"), ("RA", "R"), ("O", "")]


tests :: TestTree
tests = $(testGroupGenerator)
