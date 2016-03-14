{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import Text.XML.HXT.Core
import Data.List (nub)
import Data.List.Utils (replace)

import System.Exit
import System.Environment

type Name = String
type From = String
type To = String

data Route = Route { name :: Name, from :: From, to :: To }
           deriving (Show)


sanitize :: [Route] -> [Route]
sanitize xs = map cleanNames xs
  where
    cleanNames r = r { from = cleanFrom $ from r
                     , to = takeWhile (/=':') $ to r }
    cleanFrom = takeWhile (/='?') . replace "timer://" ""

outputGraph :: [Route] -> IO ()
outputGraph xs = do
  putStrLn "digraph G {"
  putStrLn "rankdir=LR"
  mapM_ (\(f, t) -> putStrLn (f ++ " -> " ++ t)) xs'
  putStrLn "}"
    where
      xs' = nub $ map ((,) <$> from <*> to) xs

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " xml_file"


main :: IO ()
main = do
  args <- getArgs
  case args of
    (fName:_) -> do
      xml <- readFile fName
      let doc = readString [withWarnings yes] xml
      rts <- runX $ doc >>> getRoot >>> getRoute
      outputGraph $ sanitize rts
    _ -> do
      usage
      exitFailure

getRoot = deep (isElem >>> hasName "camel:camelContext")

getRoute = proc x -> do
  rt <- deep (hasName "route" <+> hasName "camel:route" <<< isElem) -< x
  from' <- getAttrValue "uri" <<< deep (hasName "from") <<< returnA -< rt
  to' <- getAttrValue "uri" <<<
         deep (hasName "camel:to" <+> hasName "wireTap" <+> hasName "to") -< rt
  name' <- getAttrValue "id" -< rt
  returnA -< Route name' from' to'
