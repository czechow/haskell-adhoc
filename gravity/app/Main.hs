{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import Text.XML.HXT.Core

-- main :: IO ()
-- main = do
--   xml <- readFile "s1.xml"
--   let doc = readString [withWarnings yes] xml
--   res <- runX . xshow $ doc >>> indentDoc
--   putStrLn "Up and running"
--   mapM_ putStrLn res

data Guest = Guest { firstName :: String
                   , lastName :: String }
           deriving (Show, Eq)

main :: IO ()
main = do
  guests <- runX (readDocument [ withValidate no] "s3.xml"
                  >>> getGuest)
  print guests


getGuest = deep (isElem >>> hasName "guest") >>>
  proc x -> do
    fname <- getText <<< getChildren <<< deep (hasName "fname") -< x
    lname <- getText <<< getChildren <<< deep (hasName "lname") -< x
    returnA -< Guest { firstName = fname, lastName = lname }
