{-# LANGUAGE TemplateHaskell #-}

module LibTest where

import System.Random
import Data.List
import Data.Tuple.Curry
import Data.Tree
import Control.Monad.State

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Lib

test_treeBuilders :: [TestTree]
test_treeBuilders =
  [ QC.testProperty "monadic and non-monadic builder work the same" $
    \g -> unfoldTree treeBuilder ("R", 4, 3, mkStdGen g)
          QC.===
          evalState (unfoldTreeM_BF treeBuilderM ("R", 4, 3)) (mkStdGen g)
  ]

test_shuffleStrategies :: [TestTree]
test_shuffleStrategies =
  [ QC.testProperty "shuffle_slow and shuffle work the same" $
    \list g -> shuffle_slow (mkStdGen g) (list :: [Int])
               QC.=== Lib.shuffle (mkStdGen g) list
  ]

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

test_queryDeps :: [TestTree]
test_queryDeps =
  [ testCase "query empty table" $
    [] @=? queryDeps (buildDeps []) "Anything"
  , testCase "query nonexistent element" $
    [] @=? queryDeps m "Non-Existent"
  , testCase "query one element" $
    [("RA", "", 0)] @=? queryDeps m "RA"
  , testCase "query multiple elements" $
    [("R", "RA", 0), ("RA", "", 1)] @=? queryDeps m "R"
  , testCase "query table with cycles" $
    [("R", "RA", 0), ("RA", "R", 1)] @=? queryDeps m' "R"
  ]
  where
    m = buildDeps [("R", "RA"), ("RA", ""), ("O", "OA")]
    m' = buildDeps [("R", "RA"), ("RA", "R"), ("O", "")]

test_forestSearch :: [TestTree]
test_forestSearch =
  [ QC.testProperty "querying shuffled and ordered table" $
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
          sort' = sortBy (\(n1, b1, l1) (n2, b2, l2) ->
                           (l1, n1, b1) `compare` (l2, n2, b2))
      in sort' (queryDeps (buildDeps table) searchedKey)
         QC.===
         sort' (queryDeps (buildDeps shuffledTable) searchedKey)
  ]

tests :: TestTree
tests = $(testGroupGenerator)
