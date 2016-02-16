import Test.Tasty

import qualified ChannelTest


main :: IO ()
main = defaultMain $ testGroup "Tests" [ChannelTest.tests]
