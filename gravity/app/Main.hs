{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN

-- data Guest = Guest { firstName :: String
--                    , lastName :: String }
--            deriving (Show, Eq)

-- main :: IO ()
-- main = do
--   guests <- runX (readDocument [ withValidate no] "s3.xml"
--                   >>> getGuest)
--   print guests


-- getGuest = deep (isElem >>> hasName "guest") >>>
--   proc x -> do
--     fname <- getText <<< getChildren <<< deep (hasName "fname" <<< isElem) -< x
--     lname <- getText <<< getChildren <<< deep (hasName "lname") -< x
--     returnA -< Guest { firstName = fname, lastName = lname }


main :: IO ()
main = do
  xml <- readFile "s2s.xml"
  let doc = readString [withWarnings yes] xml
  eps <- runX $ doc >>> getRoot >>> getEndpoints
  rts <- runX $ doc >>> getRoot >>> getRoutes
  print eps
  print rts
  print $ length rts

getRoot = deep (isElem >>> hasName "camel:camelContext")

getEndpoints = proc x -> do
  ep <- getAttrValue "id" <<< deep (hasName "endpoint") -< x
  returnA -< ep

type Name = String
type From = String
type To = String

data Route = Route { name :: Name, from :: From, to :: To }
           deriving (Show)

getRoutes = proc x -> do
  rt <- deep (hasName "route" <+> hasName "camel:route" <<< isElem) -< x
  froms <- getAttrValue "uri" <<< deep (hasName "from") <<< returnA -< rt
  tos <- deep (hasName "to") -< rt
  rtn <- getAttrValue "id" -< rt
  tons <- listA $ getAttrValue "uri" -< tos
  returnA -< Route rtn froms (show tons)
