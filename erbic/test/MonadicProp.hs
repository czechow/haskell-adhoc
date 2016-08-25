{-# LANGUAGE TemplateHaskell #-}

module MonadicProp where

import Test.QuickCheck
import Test.QuickCheck.Monadic


-------------------------------------------------------------------------------
--                                Properties
-------------------------------------------------------------------------------

prop_monadic :: Int -> Property
prop_monadic _ = monadicIO $ do
  assert True

-------------------------------------------------------------------------------
--                                 Runner
-------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $quickCheckAll
