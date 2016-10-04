{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module MonadicProp where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Erbic.IO.Fork
import Control.Concurrent
import GHC.Conc
import qualified Data.Map.Strict as M
import Data.IORef
import qualified Data.Set as S
import System.Timeout
import Control.Exception (getMaskingState, MaskingState, MaskingState(Unmasked))

-- FIXME: figure out a way to add a timeout to the tests
-- FIXME: check async interrupt status!
-- FIXME: Thread pool: prevent adding new threads once stop is in progress (?)
-------------------------------------------------------------------------------
--                                Properties
-------------------------------------------------------------------------------

prop_tfork_ok :: Property
prop_tfork_ok = forAll gen_tfork $ \kts -> monadicIO $ do
  (tids, mvars) <- run $ unzip <$> mapM (\_ -> tfork $ do return ()) kts

  run $ mapM_ killThread $ fst . unzip $ filter (snd) $ zip tids kts

  _ <- run $ mapM readMVar mvars
  run $ allFinished tids


prop_stopThread_ok :: Property
prop_stopThread_ok = forAll gen_stopThread $ \args -> monadicIO $ do
  let (sts, kts) = unzip args
  (tids, mvars) <- run $ unzip <$> mapM (\st -> tfork $ do threadDelay st
                                                           return()) sts

  run $ mapM_ killThread $ fst . unzip $ filter (snd) $ zip tids kts

  _ <- run $ mapM readMVar mvars
  run $ allFinished tids
  assert True

prop_tfork_maskState_ok :: Property
prop_tfork_maskState_ok = forAll gen_stopThread $ \args -> monadicIO $ do
  (tids, mvars, mvsts) <-
    run $ unzip3 <$> mapM
    (\_ -> (do mvst <- newEmptyMVar :: IO (MVar MaskingState)
               (tid, mv) <- tfork $ do getMaskingState >>= putMVar mvst
                                       return()
               return (tid, mv, mvst))) args

  _ <- run $ mapM readMVar mvars
  run $ allFinished tids

  res <- run $ all (== Unmasked) <$> mapM readMVar mvsts
  assert res




prop_tpfork_ok :: Property
prop_tpfork_ok = forAll gen_tfork $ \kts -> monadicIO $ do
  let tpi = M.empty :: ThreadPoolInfo
  iortpi <- run $ newIORef tpi
  tids <- run $ mapM (\_ -> tpfork (return ()) iortpi) kts

  run $ mapM_ killThread $ fst . unzip $ filter (snd) $ zip tids kts

  (run $ timeout 1000000 $ allFinished tids) >>= \case
    Just _ -> assert True
    Nothing -> assert False

prop_tpfork_pool_ok :: Property
prop_tpfork_pool_ok = forAll gen_tfork $ \ts -> monadicIO $ do
  let tpi = M.empty :: ThreadPoolInfo
  iortpi <- run $ newIORef tpi
  (tids, mvSts, mvScs) <- run $ unzip3 <$> mapM
                          (\_ -> do mvStart <- newEmptyMVar :: IO (MVar ())
                                    mvSync  <- newEmptyMVar :: IO (MVar ())
                                    tid <- tpfork (do putMVar mvStart ()
                                                      _ <- takeMVar mvSync
                                                      return ())
                                           iortpi
                                    return (tid, mvStart, mvSync))
                          (map (const ()) ts)
  run $ mapM_ takeMVar mvSts   -- wait for all threads to start
  spawnedThreads <- run $ readIORef iortpi
  assert $ M.keysSet spawnedThreads == S.fromList tids

  run $ mapM_ (flip putMVar ()) mvScs
  run $ allFinished tids
  outstandingThreads <- run $ readIORef iortpi
  assert $ M.null outstandingThreads

prop_stopThreadPool :: Property
prop_stopThreadPool = forAll gen_tfork $ \kts -> monadicIO $ do
  let tpi = M.empty :: ThreadPoolInfo
  iortpi <- run $ newIORef tpi
  tids <- run $ mapM (\_ -> tpfork (do threadDelay $ 1000 * 1000
                                       return ()) iortpi) kts

  run $ threadDelay 100 -- FIXME: remove me after pool invariants change
  run $ stopThreadPool undefined iortpi
  tpi' <- run $ readIORef iortpi
  assert $ M.null tpi'

  (run $ timeout 1000000 $ allFinished tids) >>= \case
    Just _ -> assert True
    Nothing -> assert False

prop_tpfork_maskState_ok :: Property
prop_tpfork_maskState_ok = forAll gen_stopThread $ \args -> monadicIO $ do
  iortpi <- run $ newIORef (M.empty :: ThreadPoolInfo)
  (tids, mvsts) <-
    run $ unzip <$> mapM
    (\_ -> (do mvst <- newEmptyMVar :: IO (MVar MaskingState)
               tid <- tpfork (do getMaskingState >>= putMVar mvst
                                 return()) iortpi
               return (tid, mvst))) args

  run $ allFinished tids

  res <- run $ all (== Unmasked) <$> mapM readMVar mvsts
  assert res


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
allFinished :: [ThreadId] -> IO ()
allFinished tids = do
  ss <- mapM threadStatus tids
  if all (== ThreadFinished) ss
    then return ()
    else allFinished tids

-------------------------------------------------------------------------------
--                                 Runner
-------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $quickCheckAll
