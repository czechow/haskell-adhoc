module Lib where

import System.Random
import qualified Data.Tree as T
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State
import Data.List

type Level = Int
type Name = String
type LeafCount = Int

type Ent = String
type BenchByEnt = String

-- Take 1:
treeBuilder :: (Name, Level, LeafCount, StdGen)
            -> (String, [(Name, Level, LeafCount, StdGen)])
treeBuilder (name, level, lc, gen)
  | level <= 1 = (name, [])
  | otherwise = (name, zip4 (map ((name++) . (:[])) ['A'..])
                       (repeat (pred level))
                       (repeat lc)
                       (take len $ genFld gen'))
  where
    (len, gen') = randomR(0, lc) gen
    genFld = iterate (snd . next)

-- FIXME: Take 2 treeBuilderM should be with State monad

myTree :: T.Tree String
myTree = T.unfoldTree treeBuilder ("R", 4, 3, mkStdGen 2)

walk :: (Monoid a) => T.Tree a -> [(a, a)]
walk t@(T.Node nme _) = (nme, mempty) : walk' t []
  where
    walk' :: (Monoid b) => T.Tree b -> [(b, b)] -> [(b, b)]
    walk' (T.Node name []) acc = acc ++ [(name, mempty)]
    walk' (T.Node name forest) acc =
      acc ++
      map (\(T.Node n _) -> (name, n)) forest ++
      (concatMap (flip walk' acc) forest)

buildTable :: Name -> Level -> LeafCount -> StdGen -> [(Ent, BenchByEnt)]
buildTable rootName levels maxLeafCount g =
  walk $ T.unfoldTree treeBuilder (rootName, levels, maxLeafCount, g)

shuffle_slow :: StdGen -> [a] -> [a]
shuffle_slow _ [] = []
shuffle_slow g xs = elemDrawn : shuffle_slow g' (xs' ++ ys')
  where
    (xs', elemDrawn : ys') = splitAt rndIndex xs
    (rndIndex, g') = randomR (0, pred $ length xs) g

-- Shuffle with trees
shuffle :: StdGen -> [a] -> [a]
shuffle g = (shuffle' g) . toMap
  where
    toMap = M.fromList . (zip [0 :: Int ..])
    shuffle' g' m
      | M.null m = []
      | otherwise = snd elemDrawn : shuffle' g'' m'
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




-- This is the core problem:
-- With [(Parent, Child)] in our hands we need to locate
buildDeps :: [(Ent, BenchByEnt)] -> M.Map Ent [BenchByEnt]
buildDeps = foldl addToMap M.empty
  where
    addToMap m' (ent', bbent') = M.alter (f bbent') ent' m'
    f x' (Just xs) = Just $ x' : xs
    f x' Nothing = Just [x']


deps = buildDeps myTable


queryDeps :: M.Map Ent [BenchByEnt] -> Ent -> [(Ent, BenchByEnt, Level)]
queryDeps = queryDeps' 0 S.empty
  where
    queryDeps' lvl s m arg = case (S.member arg s, M.lookup arg m) of
      (False, Just children) ->
        map (\c -> (arg, c, lvl)) children ++
        concatMap (queryDeps' (succ lvl) (S.insert arg s) m) children
      (_, _) -> [] -- This takes care of circular dependencies too


-- now state monad, please
-- This looks more complex than param passing (unless I am missing something)
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

-- next step: add testing ???
