{-# LANGUAGE QuasiQuotes #-}

module Qq where

import Text.InterpolatedString.Perl6
import Data.List (intercalate)

bar :: Int
bar = 34

foo :: String
foo = sm [qq|This is very nice
            |Again in this area value is $bar and {bar + 3}
            |This time, I really like the result!!!|]


go :: IO()
go = putStrLn foo

sm :: String -> String
sm = intercalate "\n" . mapButFirst (stripSep . dropWhile (==' ')) . lines
  where
    stripSep ('|' : xs) = xs
    stripSep xs = xs
    mapButFirst f (x : xs) = x : map f xs
    mapButFirst _ [] = []
