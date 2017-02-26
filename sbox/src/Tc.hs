{-# LANGUAGE FlexibleInstances #-}
module Tc where

import Control.Monad.Except


data AppState lc = AppState { config :: String
                            , mutable :: [String]
                            , livyConn :: lc
                            }

type HttpCode = Int
type Login = (String, String)
type AuthToken = String

type Handler = ExceptT HttpCode IO

type SessionList = [String]

class Logic as where
  login :: as -> Login -> Handler AuthToken
  login as l = do
    return "Wania"

  livySessionList :: as -> Handler SessionList

data LivyConn = LivyConn
              deriving (Show)

callLivy :: LivyConn -> IO o -> Handler o
callLivy lc action = do
  liftIO $ putStrLn $ "Running with lc: " ++ show lc
  o' <- liftIO action
  return o'

data MockLivy = MockLivy

instance Logic (AppState LivyConn) where
  livySessionList as = callLivy (livyConn as) (return [])

instance Logic (AppState MockLivy) where
  livySessionList _ = return []
