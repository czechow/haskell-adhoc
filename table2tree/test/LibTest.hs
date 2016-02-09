{-# LANGUAGE TemplateHaskell #-}

module LibTest where

import System.Random
import Data.List
import qualified Data.Set as S
import Data.Tuple.Curry

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

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

-- FIXME: add a test with simple (?) walking of the list

test_forestSearch :: [TestTree]
test_forestSearch =
  [ QC.testProperty "searching shuffled tables should work the same" $
    \g1 g2 g3 g4 ->
      let count = fst $ randomR (1 :: Int, 10) (mkStdGen g1)
          rootNames = map (("T" ++) . show) [1 :: Int ..]
          levels = randomRs (1:: Int, 5) (mkStdGen g2)
          gens = iterate (snd . next) (mkStdGen g3)
          table = concat $ take count $
                  map (uncurryN buildTable)
                  (zip4 rootNames levels (repeat 5) gens)
          shuffledTable = shuffle (mkStdGen g4) table
          searchedKey = fst . head $ shuffledTable
      in S.fromList (queryDeps (buildDeps table) searchedKey)
         == S.fromList (queryDeps (buildDeps shuffledTable) searchedKey)
  ]

tests :: TestTree
tests = $(testGroupGenerator)
