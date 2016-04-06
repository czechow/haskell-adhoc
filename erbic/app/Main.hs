{-# LANGUAGE GADTs #-}
module Main where


data Value = I Int
           | S String
           deriving Show

data StateValue a where
  V :: a -> StateValue a
  L :: [a] -> StateValue a


type AllStates = [(String, StateValue Value)]
type PartialStates = [(String, StateValue Value)]


-- curr value, states, next value
f :: StateValue Value -> AllStates -> IO (StateValue Value)
f x _ = case x of
  V x' -> do
    putStrLn $ "Matching single value " ++ show x'
    case x' of
      I i -> return $ V $ I $ i + 2
      S s -> return $ V $ S (s ++ s)
  L xs -> do
    putStrLn $ "Matching list " ++ show xs
    return $ L xs
  --L (_: xs') -> return $ L xs'


allStates :: AllStates
allStates = [ ("First_Key", V (I 15))
            , ("Second_Key", L $ [S "x", S "y"])
            , ("Third_Key", V $ S "x")
            , ("Fourth_Key", L $ [])]

partialStates :: PartialStates
partialStates = allStates -- FIXME: hack

main :: IO ()
main = do
  putStrLn "Up and running"
  mapM_ ((flip f partialStates) . snd) allStates
