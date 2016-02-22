{-# LANGUAGE TemplateHaskell #-}

module ChannelTest where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import Control.Concurrent

import qualified Data.Map as M
import qualified Data.DList as DL

import Channel

-- For more examples on monadic testing see
-- https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Monadic.html#v:run


ms:: Integer -> Integer
ms = (*1000)

test_transfer :: [TestTree]
test_transfer =
  [ localOption (mkTimeout $ ms 1000) $
    testProperty "all data get through channel and in the same order" $
    oneWrOneRd
  , localOption (mkTimeout $ ms 1000) $
    testProperty "reading empty channel should block" $
    once $ blockingRead
  , localOption (mkTimeout $ ms 1000) $
    testProperty "multiple writers should get data through" $
    mulWrOneRd
  ]

test_dup :: [TestTree]
test_dup =
  [ localOption (mkTimeout $ ms 1000) $
    testProperty "dupped channel must see the same data as original" $
    dupOneWrMulRd
  , localOption (mkTimeout $ ms 1000) $
    testProperty
    "orig and dupped channel writes should be available in both chs" $
    dupMulWrMulRd
  ]

dupOneWrMulRd :: [Int] -> Property
dupOneWrMulRd xs = monadicIO $ do
  chOrig <- run $ newCh
  chDup <- run $ dupCh chOrig
  _ <- run $ forkIO $ sequence_ [writeCh chOrig x | x <- xs]
  ys <- run $ sequence [readCh chOrig | _ <- xs]
  zs <- run $ sequence [readCh chDup | _ <- xs]
  assert $ ys == zs

dupMulWrMulRd :: [Int] -> [Int] -> Property
dupMulWrMulRd xs ys = monadicIO $ do
  chOrig <- run $ newCh
  chDup <- run $ dupCh chOrig
  _ <- run $ forkIO $ sequence_ [writeCh chOrig (1 :: Int, x) | x <- xs]
  _ <- run $ forkIO $ sequence_ [writeCh chDup (2 :: Int, y) | y <- ys]

  zs <- run $ sequence [readCh chOrig | _ <- xs ++ ys]
  zs' <- run $ sequence [readCh chDup | _ <- xs ++ ys]

  assert $ map snd (filter ((1==) . fst) zs) == xs
  assert $ map snd (filter ((2==) . fst) zs) == ys

  assert $ (map snd zs) == (map snd zs')


-- Thread limit here makes the test run faster
mulWrOneRd :: [[Int]] -> Property
mulWrOneRd xxs = monadicIO $ do
  pre $ length xxs > 1 && length xxs <= 8 -- thread limit
  pre $ all ((>0) . length) xxs
  let txxs = zip [(1 :: Int)..] xxs
  ch <- run $ newCh
  run $ sequence_ $ [forkIO $ writeList ch thId xs | (thId, xs) <- txxs]
  ys <- run $ sequence [readCh ch | _ <- foldl (++) [] xxs]
  let zs = foldl (\m (thId, x) ->
                   M.insertWith (flip DL.append) thId (DL.singleton x) m)
           M.empty ys
  assert $ M.map DL.toList zs == M.fromList txxs
  where
    writeList _ _ [] = return ()
    writeList ch' thId' (x':xs') = do
      writeCh ch' (thId', x')
      writeList ch' thId' xs'


oneWrOneRd :: [Int] -> Property
oneWrOneRd xs = monadicIO $ do
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
tests = testGroup "ChannelTests" (test_transfer ++ test_dup)
