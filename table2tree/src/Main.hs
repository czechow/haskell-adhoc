module Main where

import System.Random
import qualified Data.Tree as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Control.Monad.State

type Level = Int
type Name = String

type Ent = String
type BenchByEnt = String

-- Take 1:
treeBuilder :: (Name, Level, StdGen) -> (String, [(Name, Level, StdGen)])
treeBuilder (name, level, gen)
  | level <= 1 = (name, [])
  | otherwise = (name, zip3 (map ((name++) . (:[])) ['A'..])
                       (repeat (pred level))
                       (take len $ genFld gen'))
  where
    (len, gen') = randomR(0, 3) gen
    genFld = iterate (snd . next)

-- FIXME: Take 2 treeBuilderM should be with State monad

myTree :: T.Tree String
myTree = T.unfoldTree treeBuilder ("R", 4, mkStdGen 2)

walk :: (Monoid a) => T.Tree a -> [(a, a)]
walk t@(T.Node nme _) = (nme, mempty) : walk' t []
  where
    walk' :: (Monoid b) => T.Tree b -> [(b, b)] -> [(b, b)]
    walk' (T.Node name []) acc = acc ++ [(name, mempty)]
    walk' (T.Node name forest) acc =
      acc ++
      map (\(T.Node n _) -> (name, n)) forest ++
      (concatMap (flip walk' acc) forest)

main :: IO ()
main = putStrLn "Up and running" >>
       putStrLn (T.drawTree myTree) >>
       putStrLn "Finished"

shuffle :: StdGen -> [a] -> [a]
shuffle _ [] = []
shuffle g xs = elemDrawn : shuffle g' (xs' ++ ys')
  where
    (xs', elemDrawn : ys') = splitAt rndIndex xs
    (rndIndex, g') = randomR (0, pred $ length xs) g

-- Shuffle with trees
shuffle2 :: StdGen -> [a] -> [a]
shuffle2 g = (shuffle2' g) . toMap
  where
    toMap = M.fromList . (zip [0 :: Int ..])
    shuffle2' g' m
      | M.null m = []
      | otherwise = snd elemDrawn : shuffle2' g'' m'
      where
        (index, g'') = randomR (0, pred $ M.size m) g'
        elemDrawn = M.elemAt index m
        m' = M.deleteAt index m


myTable :: [(Ent, BenchByEnt)]
myTable = shuffle (mkStdGen 1) $ walk myTree

myMap :: M.Map String [String]
myMap = foldl (\m (e, parent) -> M.alter (f e) parent m) M.empty myTable
  where
    f e' (Just es) = Just $ e' : es
    f e' Nothing =  Just $ [e']


-- FIXME: Any mathematical base for checking?
-- convert this to more generic type
-- Ideas:
--   Shuffle many times, check frequencies (perhaps with a hashcode)
checkShuffle :: StdGen -> [Int] -> Bool
checkShuffle _ [] = True
checkShuffle g xs = (var / fromIntegral avg) < (0.05 :: Double)
  where
    n = 10000
    gens = take n $ iterate (snd . next) g
    xxs = map (flip shuffle2 xs) gens
    sums = map sum $ L.transpose xxs
    avg = sum sums `quot` length sums
    diffs = map (subtract avg) sums
    sumsRo2 = sum $ map (^(2 :: Int)) diffs
    var = sqrt $ fromIntegral $ sumsRo2 `quot` length sums


-- This is the core problem:
-- With [(Parent, Child)] in our hands we need to locate
buildDeps :: [(Ent, BenchByEnt)] -> M.Map Ent [BenchByEnt]
buildDeps = foldl addToMap M.empty
  where
    addToMap m' (ent', bbent') = M.alter (f bbent') ent' m'
    f x' (Just xs) = Just $ x' : xs
    f x' Nothing = Just [x']


deps = buildDeps myTable


queryDeps :: M.Map Ent [BenchByEnt] -> Ent -> [(Ent, BenchByEnt)]
queryDeps = queryDeps' S.empty
  where
    queryDeps' s m arg = case (S.member arg s, M.lookup arg m) of
      (False, Just children) ->
        map ((,) arg) children ++
        concatMap (queryDeps' (S.insert arg s) m) children
      (_, _) -> [] -- This takes care of circular dependencies too


-- now state monad, please
queryDeps3 :: M.Map Ent [BenchByEnt] -> Ent -> [(Ent, BenchByEnt)]
queryDeps3 m arg = evalState (queryDeps3' m arg) S.empty

queryDeps3' :: M.Map Ent [BenchByEnt] ->
               Ent ->
               State (S.Set Ent) [(Ent, BenchByEnt)]
queryDeps3' m arg = do
  s <- get
  case (S.member arg s, M.lookup arg m) of
    (False, Just children) -> do
      put $ S.insert arg s
      -- This one is too complex to read:
      -- liftM (((map ((,) arg) children)++) . concat)
      --  (mapM (queryDeps3' m) children)
      (++) <$>
        return (map ((,) arg) children) <*>
        liftM concat (mapM (queryDeps3' m) children)
    (_, _) -> return [] -- This takes care of circular dependencies too
