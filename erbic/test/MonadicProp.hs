{-# LANGUAGE TemplateHaskell #-}

module MonadicProp where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Erbic.IO.Fork
import Control.Concurrent
import GHC.Conc
import Data.List
-------------------------------------------------------------------------------
--                                Properties
-------------------------------------------------------------------------------

prop_myfork :: Int -> Property
prop_myfork tc = monadicIO $ do
  (tids, mvars) <- run $ unzip <$> (replicateM tc' $ tfork $ do
                                       threadDelay 1000
                                       return ()
                                   )
  run $ mapM_ (\(tid, i) -> if i `mod` 1 == 0
                            then do
                              killThread tid
                              putStrLn $ "Killed thread " ++ show tid
                            else return ()) $ zip tids [1 :: Int ..]

  _ <- run $ mapM readMVar mvars
  run $ allFinished tc' tids
  assert $ True
  where
    tc' = (abs tc) `mod` 63 + 1
    allFinished :: Int -> [ThreadId] -> IO ()
    allFinished tc'' tids = do
      ss <- mapM threadStatus tids
      if all (== ThreadFinished) ss
        then return ()
        else do
          putStrLn $ "Results " ++ show tc'' ++ ": " ++ (show $ nub ss)
          allFinished tc'' tids


  -- run fork operation (no sleep inside???) =>
  -- make sure we stop the thread -> what do we want to test???
{-
type ThreadInfo = (ThreadId, MVar ())


tfork :: IO () -> IO ThreadInfo
tfork action = do
  mv <- newEmptyMVar :: IO (MVar ())
-- FIXME: if we hit here, then we have a problem...
  tid <- forkIOWithUnmask $
         \unmask -> finally (unmask action)
                            (putMVar mv ())
  return (tid, mv)

-}

prop_monadic :: Int -> Property
prop_monadic _ = monadicIO $ do
  assert True

-------------------------------------------------------------------------------
--                                 Runner
-------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $quickCheckAll
