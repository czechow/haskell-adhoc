module Chapter1
    ( myId
    , myComp
    , testMyComp
    ) where

-- Challenge 1
myId :: a -> a
myId x = x

-- Challenge 2
myComp :: (b -> c) -> (a -> b) -> (a -> c)
myComp g f = \x -> g (f x)

-- Challenge 3
testMyComp :: Bool
testMyComp =
  and $ map (\x -> myComp f myId x == myComp myId f x) [1 :: Int ..100]
  where f x = x * x
