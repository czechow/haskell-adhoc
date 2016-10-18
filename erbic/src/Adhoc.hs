module Adhoc where

import Data.List (intersperse)


data Out = Out [String]
         deriving (Show)

data Rd = Rd String
        deriving (Show)

data Kdb = Kdb String
         deriving (Show)

data File = File String
          deriving (Show)

mread :: Int -> (Int -> Rd) -> Out
mread cnt rdFn =
  let res = rdFn cnt
      transformFn (Rd s) = Out $ replicate 2 $ reverse s
  in transformFn res

kdbRead :: Kdb -> Int -> Rd
kdbRead (Kdb xs) =
  let _ =  length xs
  in \cnt -> Rd $ concat $ replicate cnt $ intersperse '-' "Wania"

fileRead :: File -> Int -> Rd
fileRead (File _) =
 \cnt -> Rd $ concat $ replicate cnt $ "Tom"
