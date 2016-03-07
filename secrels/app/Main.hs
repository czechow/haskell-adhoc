module Main where

import qualified Data.Graph as G
import Data.List
import Data.CSV
import Data.Tuple
import Data.Tuple.Extra ((&&&))
import Data.Bifunctor
import System.Exit
import Data.Ord
import Data.Either.Combinators (mapBoth, mapRight)

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Data.Set as S

type SecId = String
type BenchSecId = SecId
type SecRel = (SecId, Maybe BenchSecId)
type ErrorMsg = String
type WarnMsg = String
type RowNum = Int

-- how about throwing in some typeclasses
-- so that position in file/input config is abstracted away?

{-

1. Composition
readCsvFile - reads file - deals with IO, delivers [[String]]
            - on error - ParseError from Parsec (in IO) (composable ???)

-- This may be monadic (within a state monad)
-- and return mapping SecRel -> RowNum
-- First, let's try without a monad returning proper results
-- yes, we will be less composable (but explicit about the params)

toSecRels - converts [(RowNum, [String])] to SecRels
          - on ok - Right [(SecRel, RowNum)]
          - on error - Left [ErrorMsg]

-- again a state monad here???

checkSecRels - checks semantic correctness of SecRels [(SecRel, RowNum)]
             - on ok - Right ([SecRel], [WarnMsg])
             - on error - Left [ErrorMsg]


2. Flow control
-- This should be a composition of fns from 1.
readSecRels - reads csv file
            - on ok - IO $ Right ([SecRel], [WarnMsg])
            - on error - IO $ Left [ErrorMsg]

-- Build from csv -only if needed
-- We should declare the structure directly
fromString :: [String] -> ([SecRel], [WarnMsg])


3. Testing
--
[ "A", "B"
  "C", "D"
]

or
[("A", Just "B")
,("C", Nothing)
]

-}



secIdToIntMap :: [SecId] -> M.Map SecId Int
secIdToIntMap xs = M.fromList $ zip (sort xs) [1..]

invMap :: Ord b => M.Map a b -> M.Map b a
invMap m = M.fromList $ lst
  where
    lst = map swap $ M.toList m


{-
1. Composition
readCsvFile - reads file - deals with IO, delivers [(RowNum, [String])]
            - on error - ParseError from Parsec (in IO) (composable ???)
-}

readCsvFile :: FilePath -> HasHeader
            -> IO (Either [ErrorMsg] [(RowNum, [String])])
readCsvFile fileName NoHeader =
  mapBoth ((:[]) . show) (zip [1..]) <$> parseFromFile csvFile fileName
readCsvFile fileName SkipHeader =
  mapRight safeTail <$> readCsvFile fileName NoHeader
  where
    safeTail [] = []
    safeTail xs = tail xs


{-
-- This may be monadic (within a state monad)
-- and return mapping SecRel -> RowNum
-- First, let's try without a monad returning proper results
-- yes, we will be less composable (but explicit about the params)

toSecRels - converts [(RowNum, [String])] to SecRels
          - on ok - Right [(RowNum, SecRel)]
          - on error - Left [ErrorMsg]
-}

toSecRels :: [(RowNum, [String])] -> Either [ErrorMsg] [(RowNum, SecRel)]
toSecRels rows = foldl collect (Right []) $ map parseRow rows
  where
    -- FIXME: rewrite with nice combinators
    collect acc eith = case (acc, eith) of
      (Right rs, Right e) -> Right $ rs ++ [e]
      (Left ls, Left e) -> Left $ ls ++ [e]
      (Right _, Left e) -> Left [e]
      (Left ls, Right _) -> Left ls

parseRow :: (RowNum, [String]) -> Either ErrorMsg (RowNum, SecRel)
parseRow (rowNum, cells) =
  case (safeHead &&& safeHead . drop 16) cells of
    (Just secId, Just "") -> Right (rowNum, (secId, Nothing))
    (Just secId, Just benchSecId) -> Right (rowNum, (secId, Just benchSecId))
    (Nothing, _) ->
      Left $ "Error in row " ++ show rowNum ++ ": no first column"
    (_, Nothing) ->
      Left $ "Error in row " ++ show rowNum ++ ": no 17-th column"
  where
    safeHead [] = Nothing
    safeHead (x: _) = Just x


{-
checkSecRels - checks semantic correctness of SecRels [(SecRel, RowNum)]
             - on ok - Right ([SecRel], [WarnMsg])
             - on error - Left [ErrorMsg]
-}

-- FIXME: State monad in terms of WarnMsg?
checkSecRels :: [(RowNum, SecRel)] -> Either [ErrorMsg] ([SecRel], [WarnMsg])
checkSecRels xs =
  return xs >>= checkUniqueness >>= checkCoherence


checkUniqueness :: [(RowNum, SecRel)]
                -> Either [ErrorMsg] [(RowNum, SecRel)]
checkUniqueness xs'
  | null nonUniqueSecIds = Right xs'
  | otherwise =
      Left $ map (\(secId, rowNums) ->
                   "Duplicate secIds [" ++ secId ++ "] found in lines " ++
                   concat (intersperse ", " (map show rowNums))
                 ) $ sortBy (comparing (head . snd)) $
                     M.toList nonUniqueSecIds
  where
    nonUniqueSecIds = M.filter ((>1) . length) groupedSecIds
    groupedSecIds :: M.Map SecId [RowNum]
    groupedSecIds = foldl (\m (secId, rowNum) ->
                            M.insertWith (flip (++)) secId [rowNum] m)
                    M.empty $
                    map (fst . snd &&& fst) xs'

-- This never fails, may generate warnings
checkCoherence :: [(RowNum, SecRel)] -> Either [ErrorMsg] ([SecRel], [WarnMsg])
checkCoherence xs = Right $
  foldl (\acc (rowNum, sr@(secId, maybeBenchSecId)) ->
          case maybeBenchSecId of
            Nothing -> first (++ [sr]) acc
            Just benchSecId ->
              if S.member benchSecId secIdsSet
              then first (++ [sr]) acc
              else bimap (++ [(secId, Nothing)])
                         (++ ["Discarded incorrect ref [" ++ benchSecId
                              ++ "] in row " ++ show rowNum]) acc
        )
        ([], [])
        xs
  where
    secIdsSet = S.fromList $ map (fst . snd) xs



{-
-- This should be a composition of fns from 1.
readSecRels - reads csv file
            - on ok - IO $ Right ([SecRel], [WarnMsg])
            - on error - IO $ Left [ErrorMsg]
-}


-- FIXME: there has to be a simpler way...
readSecRels :: FilePath -> HasHeader
            -> IO (Either [ErrorMsg] ([SecRel], [WarnMsg]))
readSecRels file hasHeader =
  readCsvFile file hasHeader >>= \csv ->
    return (csv >>= toSecRels >>= checkSecRels)


buildGraph :: [(Int, Maybe Int)] -> Maybe G.Graph
buildGraph [] = Nothing
buildGraph srInt = Just $ G.buildG bounds edges'
  where
    bounds = (foldl1 min &&& foldl1 max) sInt
    sInt = map fst srInt
    edges' = foldl (\acc (s, mbs) -> case mbs of
                     Just bs -> acc ++ [(bs, s)]
                     Nothing -> acc)
             [] srInt


data HasHeader = NoHeader | SkipHeader

main :: IO ()
main = do
  putStrLn "Up and running"

  errorsOrSecRels <- readSecRels "test.csv" SkipHeader
  case errorsOrSecRels of
    Left es -> do
      putStrLn $ "Errors while processing test.csv file: "
      mapM_ (putStrLn . ("  " ++)) es
      exitWith $ ExitFailure 1
    Right (sr, warns) -> do
      mapM_ putStrLn $ ["Security structure warnings:"] ++ warns
      putStrLn ("sr has length of " ++ show (length sr))

      case processSecRels sr of
        Nothing -> putStrLn "Graph construction impossible"
        Just (g, _) ->

          putStrLn $ "Graph is: " ++ show (reverse $ sort $ map length $ G.components g)

      exitSuccess

processSecRels :: [SecRel] -> Maybe (G.Graph, M.Map SecId Int)
processSecRels srs = seqTuple (buildGraph srInt, Just s2IntMap)
  where
    s2IntMap = secIdToIntMap $ map fst srs
    srInt = map (bimap (s2IntMap M.!)
                       ((flip M.lookup s2IntMap) =<<)
                ) srs
    seqTuple (Just x, Just y) = Just (x, y)
    seqTuple (_, _) = Nothing
