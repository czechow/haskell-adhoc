module Main where

import Data.Graph
import Data.List
import Data.CSV
import Data.Tuple
import Data.Bifunctor
import System.Exit

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Data.Set as S

type SecId = String
type BenchSecId = SecId
type SecRel = (SecId, BenchSecId)
type ErrorMsg = String
type WarnMsg = String


secIdToIntMap :: [SecId] -> M.Map SecId Int
secIdToIntMap xs = M.fromList $ zip (sort xs) [1..]

invMap :: Ord b => M.Map a b -> M.Map b a
invMap m = M.fromList $ lst
  where
    lst = map swap $ M.toList m

readSecRels :: IO (Either ParseError [SecRel])
readSecRels = do
  result <- parseFromFile csvFile "test.csv"
  -- FIXME: change to a function without lambda (?)
  return $ tail . (map (\row -> (head row, head $ drop 16 row))) <$> result

checkSecRels :: [SecRel] -> Either ErrorMsg ([SecRel], [WarnMsg])
checkSecRels xs = checkUniqueKeys (xs, []) >>=
                  checkCompletness
  where
    checkUniqueKeys (xs', ws)
      | secIds == nub secIds = Right (xs', ws)
      | otherwise = Left $
                    "Non unique keys in relation: "
                    ++ show (nub (secIds \\ nub secIds))
    checkCompletness (xs', ws)
      | S.null setDiff = Right (xs', ws)
      | otherwise =
        Right (foldl (\(xs'', ws'') sr'@(s, bs) ->
                       if S.notMember bs setDiff
                       then (xs'' ++ [sr'], ws'')
                       else (xs'' ++ [(s, "")],
                             ws'' ++ ["Discarded incorrect ref [" ++ bs ++ "] in [" ++ s ++ "]"]))
               ([], ws) xs')
      where
        setDiff =
          (S.fromList $ filter (/="") benchSecIds) S.\\ S.fromList secIds
    secIds = map fst xs
    benchSecIds = map snd xs


buildGraph :: [(Int, Maybe Int)] -> Maybe Graph
buildGraph [] = Nothing
buildGraph srInt = Just $ buildG bounds edges'
  where
    bounds = (foldl1 min sInt, foldl1 max sInt)
    sInt = map fst srInt
    edges' = foldl (\acc (s, mbs) -> case mbs of
                     Just bs -> acc ++ [(bs, s)]
                     Nothing -> acc)
            [] srInt

main :: IO ()
main = do
  putStrLn "Up and running"

  errorOrSecRels <- readSecRels
  case errorOrSecRels of
    Left e -> do
      putStrLn $ "CSV parse error: " ++ show e
      exitWith $ ExitFailure 1
    Right secRels -> do
      case checkSecRels secRels of
        Left e' -> do
          putStrLn $ "Invalid security structure: " ++ e'
          exitWith $ ExitFailure 2
        Right (sr, warns) -> do
          mapM_ putStrLn $ ["Security structure warnings:"] ++ warns
          putStrLn ("sr has length of " ++ show (length sr))
          let s2IntMap = secIdToIntMap $ map fst sr
              srInt = map (bimap (s2IntMap M.!) (flip M.lookup s2IntMap)) sr

            --  dff = (S.fromList (map snd sr)) S.\\ (S.fromList (map fst sr))
              mg = buildGraph srInt
          case mg of
            Just g -> do
              putStrLn $ "Graph is: " ++ show (outdegree g)

            Nothing -> putStrLn "Graph construction impossible"
          --mapM_ (putStrLn . show) dff
          exitSuccess
