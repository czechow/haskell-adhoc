{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module AhAeson where

import GHC.Generics
import Data.Aeson
import Network.Wreq
import Network.HTTP.Client
import Control.Lens
--import Control.Monad.Cont


data Person = Person { name :: String
                     , age :: Int
                     }
            deriving (Show, Generic)


instance ToJSON Person
  -- No need to provide implementation
instance FromJSON Person

newtype Url = Url { unUrl :: String }

tUrl :: Url
tUrl = Url "http://localhost:1099"


opts :: Options
opts = defaults & (manager .~ Left (defaultManagerSettings {
                                       managerResponseTimeout = Nothing }))
                                           --Just $ 1000 * 1000 }))

run :: IO ()
run = do
  res <- getWith opts $ unUrl tUrl
  putStrLn $ "Got: " ++ show res
  return ()

main :: IO ()
main = run

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes a@(_:xs) = a : suffixes xs

data T a = E
         | T (T a) a (T a)
         deriving (Show)

t :: T Int
t = T E 2 E

t2 :: T Int
t2 = insert (insert (insert E (2 :: Int)) 1) 3

member :: Ord a => T a -> a -> Bool
member E _ = False
member (T l el r) x = if x < el then member l x
                      else if x > el
                           then member r x
                           else True

insert :: Ord a => T a -> a -> T a
insert E x = T E x E
insert a@(T l el r) x = if x == el then a
                        else if x < el then T (insert l x) el r
                             else T l el (insert r x)

member2 :: Ord a => T a -> a -> Bool
member2 E _ = False
member2 a@(T _ el _) x = innMemb a x el
  where
    innMemb E x' tel' = x' == tel'
    innMemb (T l' el' r') x' tel' = if x' <= el' then innMemb l' x' el'
                                    else innMemb r' x' tel'
