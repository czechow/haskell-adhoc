{-# LANGUAGE ScopedTypeVariables #-}
module MndsIO where

import Control.Monad.Except
import Control.Exception (try, SomeException, IOException)


type MndIO = ExceptT String IO

type Session = String
type Url = String


liftIOwithCatch :: IO a -> MndIO a
liftIOwithCatch action = do
  res <- liftIO $ try action
  case res of
    Right r -> return r
    Left (e :: IOException) -> throwError $ show e


newSession' :: Url -> MndIO Session
newSession' _ = do
  _ <- liftIOwithCatch $ readFile "sdfdsf"
  return "xxx"
