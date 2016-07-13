{-# LANGUAGE TemplateHaskell #-}

module Erbic.Data.Msg.ScanMsgProp where


import Erbic.Data.Msg.ScanMsg

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Test.QuickCheck

-------------------------------------------------------------------------------
--                                Properties
-------------------------------------------------------------------------------

prop_1 :: Property
prop_1 = forAll genInput3 $ \(xs, buff, pp) ->
  let (res, (ScanData lo _ _ _)) = runScan xs $ mkScanData buff pp
  in (not $ sep `isInfixOf` lo)
     &&
     (not $ sep `isInfixOf` concat res)

prop_2 :: Property
prop_2 = forAll genNoOverflow $ \(xs, buff) ->
  let (rss, (ScanData lo _ _ _)) = runScan xs $ mkScanData buff PPIn
  in concat (map (++sep) rss) ++ lo == buff ++ xs

prop_3 :: Property
prop_3 =
  forAll genNoOverflow $ \(xs, buff) ->
  let (rss, (ScanData lo _ _ sc)) = runScan xs $ mkScanData buff PPOut
  in concat rss ++ lo == (concat . tail $ splitOn sep (buff ++ xs))
     &&
     sc == if sep `isInfixOf` (buff ++ xs) then 1 else 0

prop_4 :: Property
prop_4 = forAll genOverflow $ \(xs, buff, pp) ->
  let (rss, (ScanData lo _ ovr sc)) = runScan xs $ mkScanData buff pp
      input' = drop ((length $ buff ++ xs) - maxBuffLen) (buff ++ xs)
      (rss', (ScanData lo' _ _ sc')) = runScan input' $ mkScanData "" PPOut
  in ovr == 1 && sc == sc' &&
     rss == rss' && lo == lo'

-------------------------------------------------------------------------------
--                                Generators
-------------------------------------------------------------------------------

genInput :: Gen String
genInput = concat <$> listOf (oneof [elements (map (:[]) ['a'..'z']),
                                     return sep])

genNoOverflow :: Gen (String, String)
genNoOverflow = do
  xs <- resize maxBuffLen genInput
  p <- choose (0, length xs)
  return $ splitAt p xs

genOverflow :: Gen (String, String, PacketParse)
genOverflow = do
  xs <- resize (5 * maxBuffLen) genInput
  p <- choose (0, length xs)
  pp <- elements [PPIn, PPOut]
  if length xs <= maxBuffLen
    then genOverflow
    else let (x, y) = splitAt p xs
         in return (x, y, pp)

genInput2 :: Gen (String, String)
genInput2 = (,) <$> genInput <*> elements (map (:[]) ['a'..'z'])

genInput3 :: Gen (String, String, PacketParse)
genInput3 = (,,) <$> genInput
                 <*> genInput
                 <*> elements [PPIn, PPOut]

-------------------------------------------------------------------------------
--                                 Runner
-------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $quickCheckAll
