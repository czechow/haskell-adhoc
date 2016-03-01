module Main where

import Data.Graph
import Data.List
import Data.CSV
import Data.Tuple
import Data.Tuple.Extra ((&&&))
import Data.Bifunctor
import System.Exit
import Data.Ord

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Data.Set as S

type SecId = String
type BenchSecId = SecId
type SecRel = (SecId, BenchSecId)
type ErrorMsg = String
type WarnMsg = String
type RowNum = Int

-- FIXME:? or Just record type?

{-
1. Composition
readCsvFile - reads file - deals with IO, delivers [[String]]
            - on error - returns IO or ParseError from Parsec? (FIXME)


toSecRels - tries to convert [[String]] to (SecRels)
          - on error - returns Left

checkSecRels - checks sematic correctness of SecRels


-}



secIdToIntMap :: [SecId] -> M.Map SecId Int
secIdToIntMap xs = M.fromList $ zip (sort xs) [1..]

invMap :: Ord b => M.Map a b -> M.Map b a
invMap m = M.fromList $ lst
  where 
    lst = map swap $ M.toList m


readCsvFile :: FilePath -> IO (Either ParseError [[String]])
readCsvFile fileName = parseFromFile csvFile fileName


toSecRels :: [[String]] -> Either ErrorMsg [SecRel]
toSecRels xxs = undefined

readSecRels :: FilePath -> HasHeader -> IO (Either ErrorMsg [(RowNum, SecRel)])
readSecRels fileName hasHeader = do
  csv <- parseFromFile csvFile fileName
  return $ verifyStructure csv hasHeader
    where
      verifyStructure (Left parseError) _ = Left $ show parseError
      verifyStructure (Right rows) NoHeader =
        sequence $ map verifyRow $ zip [1..] rows
      verifyStructure (Right rows) SkipHeader =
        sequence $ map verifyRow $ safeTail $ zip [1..] rows

      verifyRow (rowNum, cells) =
        case (safeHead &&& safeHead . drop 16) cells of
          (Just secId, Just benchSecId) -> Right $ (rowNum, (secId, benchSecId))
          (Nothing, _) ->
            Left $ "Error in row " ++ show rowNum ++ ": no first column"
          (_, Nothing) ->
            Left $ "Error in row " ++ show rowNum ++ ": no 17-th column"

      safeTail [] = []
      safeTail xs = tail xs

      safeHead [] = Nothing
      safeHead (x: _) = Just x


-- FIXME: State monad in terms of WarnMsg
checkSecRels :: [(RowNum, SecRel)] -> Either [ErrorMsg] ([SecRel], [WarnMsg])
checkSecRels xs = do
  case checkUniqueSecIds xs of
    Left e -> Left e
    -- FIXME: finished here
    

checkUniqueSecIds :: [(RowNum, SecRel)]
                  -> Either [ErrorMsg] ([SecRel], [WarnMsg])
checkUniqueSecIds xs'
  | null nonUniqueSecIds = Right ((map snd xs'), [])
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

checkSecRelsCompletness :: ([(RowNum, SecRel)]) -> ([SecRel], [WarnMsg])
checkSecRelsCompletness xs =
  foldl (\acc (rowNum, sr@(_, benchSecId)) ->
          if S.notMember benchSecId secIdsSet
          then first (++ [sr]) acc
          else bimap (++ [sr])
                     (++ ["Discarded incorrect ref [" ++ benchSecId
                          ++ "] in row " ++ show rowNum])
                     acc)
        ([], [])
        xs
  where
    secIdsSet = S.fromList $ map (fst . snd) xs

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


data HasHeader = NoHeader | SkipHeader

main :: IO ()
main = do
  putStrLn "Up and running"

  errorOrSecRels <- readSecRels "test.csv" SkipHeader
  case errorOrSecRels of
    Left e -> do
      putStrLn $ "CSV parse error: " ++ show e
      exitWith $ ExitFailure 1
    Right secRels -> do
      case checkSecRels secRels of
        Left e' -> do
          putStrLn $ "Invalid security structure: "
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
