module Main where


import Criterion.Main
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary (Arbitrary)

--import MyFree

fib :: Integer -> Integer
fib m | m < 0 = error "negative!"
      | m == 0 = 0
      | m == 1 = 1
      | otherwise = fib (m-1) + fib (m-2)

--main :: IO ()
--main = putStrLn "Up and running"


main :: IO ()
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "11" $ whnf fib 11
               , bench "30" $ whnf fib 30
               ]
  ]

data TestStatement = TS { code :: String
                        , timeout :: Int
                        }
                   deriving Show

mkTestStatement :: Int -> Int -> TestStatement
mkTestStatement sc pl =
  let c = "select /*+ parallel (d, " ++ show pl ++
          ") */ from d where rownum < " ++ show sc
  in TS c 3600

data TestCommand = TC TestStatement
                 | CloseSession
                 | DelaySecs Int
                 deriving Show

instance Arbitrary TestStatement where
  arbitrary = do
    sc <- QC.choose (1024, 2048)
    pl <- QC.elements $ map (2^) rng
    return $ mkTestStatement sc pl
      where
        rng = [0 .. 4] :: [Int]

instance Arbitrary TestCommand where
  arbitrary = QC.frequency [ (5, pure CloseSession)
                           , (10, DelaySecs <$> QC.choose (0, 30))
                           , (85, TC <$> QC.arbitrary )]
