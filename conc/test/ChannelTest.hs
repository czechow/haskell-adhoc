{-# LANGUAGE TemplateHaskell #-}

module ChannelTest where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import System.Process
import Control.Concurrent
import Channel

-- For more examples on monadic testing see
-- https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Monadic.html#v:run

class EOFy a where
  eof :: a

instance EOFy Int where
  eof = -1

ms:: Integer -> Integer
ms = (*1000)

test_transfer :: [TestTree]
test_transfer =
  [ localOption (mkTimeout $ ms 1000) $
    testProperty "all data get through channel and in the same order" $
    transferOneThread
  , localOption (mkTimeout $ ms 1000) $
    testProperty "reading empty channel should block" $
    once $ blockingRead
  ]

transferOneThread :: [Int] -> Property
transferOneThread xs = monadicIO $ do
  ch <- run $ newCh
  _ <- run $ forkIO $ sequence_ [writeCh ch x | x <- xs]
  zs <- run $ sequence [readCh ch | _ <- xs]
  assert (zs == xs)

blockingRead :: Property
blockingRead = monadicIO $ do
  ch <- run $ newCh
  readFinished <- run $ newMVar False
  _ <- run $ forkIO $ do
    _ <- readCh ch
    _ <- takeMVar readFinished
    putMVar readFinished True
  run $ threadDelay 20
  v <- run $ readMVar readFinished
  assert $ not v
  run $ writeCh ch "xxx"
  run $ threadDelay 20
  v' <- run $ readMVar readFinished
  assert v'




tests :: TestTree
tests = testGroup "ChannelTests" (test_transfer)
