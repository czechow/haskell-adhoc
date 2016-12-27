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
import Control.Concurrent (threadDelay)
--       (threadDelay, forkIO, ThreadId, killThread, myThreadId, forkFinally)
import Control.Monad (void)
import qualified Data.Set as S
import Data.List (intercalate)
import qualified Control.Concurrent.Async as A
--import Control.Exception (finally, bracket, bracket_)

import Model.Rest


-- FIXME: change me to Control.Concurrent.Async

data SessionsState a = SessionsState { nextId :: SessionId
                                     , sessionMap :: M.Map SessionId (Session a)
                                     }
                     deriving (Show)


createSession :: SessionConfig -> SessionsState a
              -> (SessionsState a, Session a)
createSession (SessionConfig k pu c) (SessionsState sId sm) =
  (SessionsState sId' (M.insert sId s sm), s)
  where
    s = Session sId k pu c NOT_STARTED
    sId' = SessionId $ succ $ unSessionId sId


deleteSession :: SessionId -> SessionsState a
              -> (SessionsState a, Maybe SessionId)
deleteSession sId ss
  | M.member sId sm = (ss { sessionMap = M.delete sId sm }, Just sId)
  | otherwise = (ss, Nothing)
  where
    sm = sessionMap ss

data SessionStatesFrom = AllStates
                       | Set (S.Set SessionState)
                       deriving (Show, Eq, Ord)


-- FIXME: lenses would be nice here
changeSessionState :: SessionId
                   -> SessionStatesFrom
                   -> SessionState
                   -> SessionsState a
                   -> (SessionsState a, Maybe SessionState)
changeSessionState sId fromStates toState ss =
  case M.lookup sId sm of
    Just session -> case fromStates of
      AllStates -> success session
      Set allowedStates -> if S.member (state session) allowedStates
                           then success session
                           else failure
    Nothing -> failure
  where
    sm = sessionMap ss
    success s' =
      (ss { sessionMap = M.adjust (f toState) sId sm }, Just (state s'))
    failure = (ss, Nothing)
    f st' s' = s' { state = st' }


doStartSession :: Session a -> IORef (SessionsState a) -> IO ()
doStartSession s ior = do
  threadDelay $ 1000 * 1000 * 1
  void $ atomicModifyIORef ior (changeSessionState (id s) AllStates STARTING)
  threadDelay $ 1000 * 1000 * 50
  void $ atomicModifyIORef ior
    (changeSessionState (id s) (Set $ S.singleton STARTING) IDLE)


initialSessionsState :: SessionsState a
initialSessionsState = SessionsState (SessionId 0) M.empty


emptyWorkerPool :: M.Map SessionId (A.Async ())
emptyWorkerPool = M.empty


workerPoolScanner :: IORef (M.Map SessionId (A.Async ()))
                  -> IORef (SessionsState a)
                  -> IO ()
workerPoolScanner wpr ssr = do
  wp <- readIORef wpr
  sIds <- M.keysSet . sessionMap <$> readIORef ssr
  putStrLn $ "Sessions: " ++
             (intercalate "," $ map (show . unSessionId) $ S.toList sIds)
  putStrLn $ "Workers:  " ++
             (intercalate "," $ map (show . unSessionId) $
                              S.toList . M.keysSet $ wp)
  void $ A.mapConcurrently (dispose wp) $ S.toList (M.keysSet wp S.\\ sIds)
  threadDelay $ 1000 * 1000 * 1
  workerPoolScanner wpr ssr
  where
    dispose wp' sId = case M.lookup sId wp' of
      Just asc -> do
        A.cancel asc
        atomicModifyIORef' wpr $ \m -> (M.delete sId m, ())
      Nothing -> return ()


newWorker :: IORef (M.Map SessionId (A.Async ())) -> SessionId -> IO ()
          -> IO (A.Async ())
newWorker wpr sId action = do
  a <- A.async action
  atomicModifyIORef' wpr $ \m -> (M.insert sId a m, ())
  return a

main :: IO ()
main = do
  sessionsRef <- newIORef initialSessionsState
  workerPoolRef <- newIORef emptyWorkerPool
  A.withAsync (workerPoolScanner workerPoolRef sessionsRef)
              (\_ -> action sessionsRef workerPoolRef)
  where
    action sessionsRef workerPoolRef = do
      scotty 8998 $ do
        get "/sessions" $ do
          ss <- liftIO $
                map snd . M.toAscList . sessionMap <$> readIORef sessionsRef
          WS.json $ Sessions (length ss) ss

        get "/sessions/:n" $ do
          sId <- WS.param "n"
          s <- liftIO $
               M.lookup (SessionId sId) . sessionMap <$> readIORef sessionsRef
          case s of
            Just s' -> WS.json s'
            Nothing -> status $ HT.Status 404 $
                              T.pack $ "No session found with id " ++ show sId
        delete "/sessions/:n" $ do
          n <- WS.param "n"
          m <- liftIO $
               atomicModifyIORef sessionsRef $ deleteSession $ SessionId n
          case m of
            Just _ -> WS.json $ SessionDeleted "deleted"
            Nothing -> next

        post "/sessions" $ do
          sc <- WS.jsonData :: ActionM SessionConfig
          s <- liftIO $ atomicModifyIORef sessionsRef $ createSession sc
          liftIO $ void $ newWorker workerPoolRef (id s) $
            doStartSession s sessionsRef
          WS.json s
