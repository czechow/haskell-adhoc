{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- Based on
-- http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html
-- https://github.com/scotty-web/scotty/wiki

module Main where

import Prelude hiding (id)
import Web.Scotty as WS
import qualified Data.Map.Strict as M
import Data.IORef
import Control.Monad.IO.Class
import qualified Network.HTTP.Types as HT
import qualified Data.ByteString.Char8 as T

import Model.Rest


data SessionsState a = SessionsState { nextId :: SessionId
                                     , sessionMap :: M.Map SessionId (Session a)
                                     }
                     deriving (Show)


createSession :: SessionConfig -> SessionsState a
              -> (SessionsState a, Session a)
createSession (SessionConfig k pu c) (SessionsState sId sm) =
  (SessionsState sId' (M.insert sId s sm), s)
  where
    s = Session sId k pu c
    sId' = SessionId $ succ $ unSessionId sId

-- FIXME: change to record syntax
deleteSession :: SessionId -> SessionsState a
              -> (SessionsState a, Maybe SessionId)
deleteSession sId (SessionsState x sm)
  | M.member sId sm = (SessionsState x (M.delete sId sm), Just sId)
  | otherwise = (SessionsState x sm, Nothing)


initialSessionsState :: SessionsState a
initialSessionsState = SessionsState (SessionId 0) M.empty


main :: IO ()
main = do
  sessionsRef <- newIORef initialSessionsState
  scotty 8998 $ do
    get "/sessions" $ do
      ss <-
        liftIO $ map snd . M.toAscList . sessionMap <$> readIORef sessionsRef
      WS.json $ Sessions (length ss) ss
    get "/sessions/:n" $ do
      sId <- WS.param "n"
      s <-
        liftIO $ M.lookup (SessionId sId) . sessionMap <$> readIORef sessionsRef
      case s of
        Just s' -> WS.json s'
        Nothing -> status $ HT.Status 404 $
                            T.pack $ "No session found with id " ++ show sId
    delete "/sessions/:n" $ do
      n <- WS.param "n"
      m <- liftIO $ atomicModifyIORef sessionsRef $ deleteSession $ SessionId n
      case m of
        Just _ -> WS.json $ SessionDeleted "deleted"
        Nothing -> next

    post "/sessions" $ do
      sc <- WS.jsonData :: ActionM SessionConfig
      s <- liftIO $ atomicModifyIORef sessionsRef $ createSession sc
      WS.json s

-- ============================================================================
--                                Helpers
-- ============================================================================

lookupMaxKey :: Ord k => M.Map k a -> Maybe k
lookupMaxKey m
  | M.null m = Nothing
  | otherwise = Just $ maximum $ M.keysSet m
