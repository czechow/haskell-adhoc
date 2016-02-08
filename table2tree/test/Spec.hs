import Test.Tasty

import qualified LibTest

-- For more information on testing see https://github.com/feuerbach/tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" [LibTest.tests]
