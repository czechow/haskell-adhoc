module Main where

import Text.XML.HXT.Core

main :: IO ()
main = do
  xml <- readFile "s1.xml"
  let doc = readString [withWarnings yes] xml
  res <- runX . xshow $ doc >>> indentDoc
  putStrLn "Up and running"
  mapM_ putStrLn res
