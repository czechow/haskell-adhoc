{-# LANGUAGE TemplateHaskell #-}

module MonadicProp where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Erbic.IO.Fork
import Control.Concurrent
import GHC.Conc
-------------------------------------------------------------------------------
--                                Properties
-------------------------------------------------------------------------------

prop_myfork :: Property
prop_myfork = forAll genInput $ \(tc, bs) ->  monadicIO $ do
  (tids, mvars) <- run $ unzip <$> (replicateM tc $ tfork $ do
                                       return ())
  let (ktids, _) = unzip $ filter (snd) $ zip tids bs

  run $ mapM_ killThread ktids

  _ <- run $ mapM readMVar mvars
  res <- run $ allFinished tc tids
  assert res
  where
    allFinished :: Int -> [ThreadId] -> IO Bool
    allFinished tc' tids = do
      ss <- mapM threadStatus tids
      if all (== ThreadFinished) ss
        then return True
        else allFinished tc' tids

-------------------------------------------------------------------------------
--                               Generators
-------------------------------------------------------------------------------

genInput :: Gen (Int, [Bool])
genInput = do
  n <- choose(1, 64)
  bs <- vectorOf n $ elements [False, True]
  return (n, bs)


-------------------------------------------------------------------------------
--                                 Runner
-------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $quickCheckAll
