{-# LANGUAGE TemplateHaskell #-}

module MonadicProp where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Erbic.IO.Fork
import Control.Concurrent
import GHC.Conc
import qualified Data.Map.Strict as M
import Data.IORef
import qualified Data.Set as S

-------------------------------------------------------------------------------
--                                Properties
-------------------------------------------------------------------------------

prop_tfork_ok :: Property
prop_tfork_ok = forAll gen_tfork $ \kts -> monadicIO $ do
  (tids, mvars) <- run $ unzip <$> mapM (\_ -> tfork $ do return ()) kts

  run $ mapM_ killThread $ fst . unzip $ filter (snd) $ zip tids kts

  _ <- run $ mapM readMVar mvars
  af <- run $ allFinished tids
  assert af


prop_stopThread_ok :: Property
prop_stopThread_ok = forAll gen_stopThread $ \args -> monadicIO $ do
  let (sts, kts) = unzip args
  (tids, mvars) <- run $ unzip <$> mapM (\st -> tfork $ do threadDelay st
                                                           return()) sts

  run $ mapM_ killThread $ fst . unzip $ filter (snd) $ zip tids kts

  _ <- run $ mapM readMVar mvars
  af <- run $ allFinished tids
  assert af
  assert True


prop_tpfork_ok :: Property
prop_tpfork_ok = forAll gen_tfork $ \kts -> monadicIO $ do
  let tp = M.empty :: ThreadPoolInfo
  iortp <- run $ newIORef tp
  -- Create IORef
  -- Spawn n threads with tp method => give them long times (1s)
  --
  tids <- run $ mapM (\_ -> tpfork
                            (do putStrLn "Spawned"
                                threadDelay 10000000
                                return ()) iortp) [1 :: Int .. 3]

  run $ threadDelay 100000
  spawnedThreads <- run $ readIORef iortp
  let st = M.keys spawnedThreads
  let ts = S.fromList tids
  run $ putStrLn ("spawnedThreads: " ++ show (length st) ++ " " ++
                  show (M.keys spawnedThreads))
  run $ putStrLn ("tids: " ++ show (length ts) ++ " " ++
                  show (S.fromList tids))
  assert $ S.fromList (M.keys spawnedThreads) == S.fromList tids

  run $ mapM_ killThread $ fst . unzip $ filter (snd) $ zip tids kts

  --_ <- run $ mapM readMVar mvars
  af <- run $ allFinished tids
  assert af

-------------------------------------------------------------------------------
--                               Generators
-------------------------------------------------------------------------------

gen_tfork :: Gen [Bool]
gen_tfork = do
  n <- choose(1, 64)
  bs <- vectorOf n $ elements [False, True]
  return bs

gen_stopThread :: Gen [(Int, Bool)]
gen_stopThread = do
  bs <- gen_tfork
  sts <- vectorOf (length bs) $ elements $ map (10^) [0 :: Int .. 3]
  return $ zip sts bs
-------------------------------------------------------------------------------
--                                Helpers
-------------------------------------------------------------------------------
allFinished :: [ThreadId] -> IO Bool
allFinished tids = do
  ss <- mapM threadStatus tids
  if all (== ThreadFinished) ss
    then return True
    else allFinished tids

-------------------------------------------------------------------------------
--                                 Runner
-------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $quickCheckAll
