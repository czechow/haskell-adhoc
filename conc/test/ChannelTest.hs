{-# LANGUAGE TemplateHaskell #-}

module ChannelTest where

import Test.Tasty
--import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
--import Test.QuickCheck

import System.Process

import Channel


class EOFy a where
  eof :: a

instance EOFy Int where
  eof = -1


test_transfer :: [TestTree]
test_transfer =
  [ testProperty "all data get through channel and in the same order" $
    prop_factor
  ]

-- $ factor 16
-- 16: 2 2 2 2
factor :: Integer -> IO [Integer]
factor n = parse `fmap` readProcess "factor" [show n] "" where

  parse :: String -> [Integer]
  parse = map read . tail . words

prop_factor :: Positive Integer -> Property
prop_factor (Positive n) = monadicIO $ do
  factors <- run (factor n)
  assert (product factors == n)

test_props :: [TestTree]
test_props =
  [ testProperty "XXX" $
    \a b -> a == (5 :: Int) && b == (6 :: Int)
  , testProperty "" $ monadicIO $ do
      assert False
  ]


writerFunction :: Ch a -> [a] -> IO ()
writerFunction channel xs = mapM_ (writeCh channel) xs


tests :: TestTree
tests = testGroup "All" (test_transfer ++ test_props)
