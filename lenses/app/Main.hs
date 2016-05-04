{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Lens
import Control.Monad.State

newtype S1 = S1 String deriving Show

data AllStates = AllStates { _s1 :: S1
                           , _s2 :: Int
                           , _s3 :: Bool
                           , _s4 :: String
                           }
                 deriving Show

makeLenses ''AllStates

allStates :: AllStates
allStates = AllStates (S1 "Wania") 16 True "Warsaw"

setToState :: AllStates -> a -> Lens' AllStates a -> AllStates
setToState st x l = set l x st

getFromState :: AllStates ->  Lens' AllStates a -> a
getFromState st l = view l st



newtype I1 = I1 Int deriving Show
newtype I2 = I2 String deriving Show
newtype I3 = I3 String deriving Show
newtype O2 = O2 Int deriving Show
newtype S4 = S4 () deriving Show




proc2 :: (I1, I2) -> State AllStates O2
proc2 (I1 i1, I2 _) = do
  st <- get
  let (S1 s) = view s1 st
      o = length s + i1
      ns = s  ++ " [touched here]"
  put $ set s1 (S1 ns) st
  return $ O2 o

modifyState :: (s -> (o, s)) -> Lens' AllStates s -> AllStates -> (o, AllStates)
modifyState f l as = let s = view l as
                         (o, ns) = f s
                     in (o, set l ns as)

-- modifyState2 :: (s -> (o, s)) -> Lens' AllStates s -> State AllStates o
-- modifyState2 f l = do
--   as <- get
--   let s = view l as
--       (o, ns) = f s
--   put $ set l ns as
--   return o


fS1 :: (I1, I2) -> S1 -> (O2, S1)
fS1 (I1 i1, I2 _) (S1 s) =
  (O2 $ length s + i1, S1 $ s ++ " [touched here]")



runRule :: i -> (i -> s -> (o, s)) -> Lens' AllStates s -> State AllStates o
runRule inputs fS l = do
  st <- get
  let (o, nst) = modifyState (fS inputs) l st
  put nst
  return $ o


allLogic :: Maybe I1 -> Maybe I2 -> State AllStates O2
allLogic mi1 mi2 = do
  case (mi1, mi2) of
    (Just i1, Just i2) -> do
      _ <- runRule (i1, i2) fS1 s1
      _ <- runRule (i1, i2) fS1 s1
      o3 <- runRule (i1, i2) fS1 s1
      return o3
    (_, _) -> undefined





proc3 :: (I1, I2) -> State AllStates O2
proc3 inputs = do
  st <- get
  let (o, nst) = modifyState (fS1 inputs) s1 st
  put nst
  return $ o


runO2 :: O2 -> Int
runO2 (O2 x) = x

allFn :: Int -> Int
allFn n =
  sum $ map (\i -> runO2 $ fst $ runState (proc2 (I1 i, I2 "x")) allStates)
            [1..n]

allFn2 :: Int -> Int
allFn2 n =
  sum $ map (\i -> let (S1 s1') = _s1 $ snd $ runState (proc2 (I1 i, I2 "x")) allStates
                   in length s1')
            [1..n]


--oper :: AllStates -> (AllStates -> (o, AllStates)) -> (o, AllStates)

--allLogic :: Maybe I1 -> Maybe I2 -> State AllStates O2

main :: IO ()
main = do
  let (o2, ns) = runState (allLogic (Just $ I1 13) (Just $ I2 "")) allStates
  putStrLn $ show o2
  putStrLn $ show ns
-- main :: IO ()
-- main = do
--   let n = 10000000
--   putStrLn $ "Up and running for n = " ++ show n
--   putStrLn $ show $ allFn2 n
