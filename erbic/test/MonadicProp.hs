{-# LANGUAGE TemplateHaskell #-}

module MonadicProp where

import Test.QuickCheck
import Test.QuickCheck.Monadic


-------------------------------------------------------------------------------
--                                Properties
-------------------------------------------------------------------------------

--prop_monadic :: Property
prop_monadic x = monadicIO $ do
  assert True

-------------------------------------------------------------------------------
--                                 Runner
-------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $quickCheckAll
