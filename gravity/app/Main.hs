{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import Text.XML.HXT.Core
import Data.List (nub)

type Name = String
type From = String
type To = String

data Route = Route { name :: Name, from :: From, to :: To }
           deriving (Show)


sanitize :: [Route] -> [Route]
sanitize xs = map cleanNames xs
  where
    cleanNames r = r { to = (takeWhile (/=':') $ to r) }

outputGraph :: [Route] -> IO ()
outputGraph xs = do
  putStrLn "digraph G {"
  putStrLn "rankdir=LR"
  mapM_ (\(f, t) -> putStrLn (f ++ " -> " ++ t)) xs'
  putStrLn "}"
    where
      xs' = nub $ map ((,) <$> from <*> to) xs

main :: IO ()
main = do
  xml <- readFile "s2.xml"
  let doc = readString [withWarnings yes] xml
  rts <- runX $ doc >>> getRoot >>> getRoute -- This gets me the whole struct

  outputGraph $ sanitize rts

getRoot = deep (isElem >>> hasName "camel:camelContext")

getRoute = proc x -> do
  rt <- deep (hasName "route" <+> hasName "camel:route" <<< isElem) -< x
  from' <- getAttrValue "uri" <<< deep (hasName "from") <<< returnA -< rt
  to' <- getAttrValue "uri" <<< deep (hasName "to" <+> hasName "wireTap") -< rt
  name' <- getAttrValue "id" -< rt
  returnA -< Route name' from' to'
