{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Erbic.In.SockService.SockMsgService2 where


import Control.Concurrent hiding (writeChan)
import Control.Exception
import Network.Socket
import qualified Data.Map.Strict as M
import GHC.IO.Exception
import Erbic.IO.Fork
import Data.IORef

import Control.Concurrent.BoundedChan
import Data.List (intercalate)
import Erbic.Data.Msg.ScanMsg



data SSInitData s l = SSInitData String l

data SSData s l = SSData { name :: String
                         , sock :: s
                         , logger :: l
                         , svcData :: (ThreadId, MVar ())
                         , connData :: IORef (M.Map ThreadId (MVar ())) }

type ErrMsg = String

data ReadRes = RRData String
             | RRClosed
             | RRError (Maybe Int) ErrMsg
             deriving Show


class Logger l where
  lWrite :: l -> String -> IO ()

instance Logger (BoundedChan String) where
  lWrite ch msg = writeChan ch msg


class (Show s, Logger l) => Service s l where
  ssOpen :: l -> IO s
  ssClose :: s -> l -> IO ()
  ssAccept :: s -> l -> IO (s, String)
  ssRead :: s -> Int -> l -> IO ReadRes

  newSS :: HostName -> Int -> l -> IO (SSInitData s l)
  newSS _ _ l = return $ SSInitData "SockSrvName" l


  startSS :: SSInitData s l -> IO (SSData s l)
  startSS (SSInitData nme l) = do
    iorConnData <- newIORef M.empty
    bracketOnError (ssOpen l :: IO s)
                   (flip ssClose l) $ \s ->
      bracketOnError (tfork $ acceptLoop s iorConnData l)
                     (stopThread 0) $
                     \(tid, mv) -> return $ SSData { name = nme
                                                   , sock = s
                                                   , logger = l
                                                   , svcData = (tid, mv)
                                                   , connData = iorConnData
                                                   }

  stopSS :: Int -> SSData s l -> IO ()
  stopSS t ssd =
    mask_ $ do let (tid, mv) = svcData ssd
               lWrite (logger ssd) $
                      "Stopping service " ++ name ssd
               stopThread t (tid, mv)
               lWrite (logger ssd) $
                      "Accept thread " ++ show tid ++ " stopped"
               tidsMvsMap <- readIORef $ connData ssd
               lWrite (logger ssd) $
                      "Now stopping connections: " ++ (show $ M.keys tidsMvsMap)
               stopThreadPool t $ connData ssd
               lWrite (logger ssd) $ "All connections stopped"
               ssClose (sock ssd) (logger ssd)
               lWrite (logger ssd) $ "Service " ++ name ssd ++ " stopped"


  isSSRunning :: SSData s l -> IO Bool
  isSSRunning ssd =
    tryReadMVar (snd $ svcData ssd) >>= \case Just _ -> return False
                                              Nothing -> return True


acceptLoop :: Service ss l => ss -> IORef ThreadPoolInfo -> l -> IO ()
acceptLoop s mvTids l = do
  bracketOnError (ssAccept s l) (flip ssClose l . fst) $
     \(s', sa') -> mask_ $ do _ <- tpfork (serveConnection s' sa' l) mvTids
                              return ()
  acceptLoop s mvTids l


serveConnection :: Service ss l => ss -> String -> l -> IO ()
serveConnection s sa l = do
  finally (do tid <- myThreadId
              lWrite l $ "New connection from " ++ show sa ++
                         " on " ++ show s ++
                         " served in " ++ show tid
              doMsgReception s l)
          (do ssClose s l
              tid <- myThreadId
              lWrite l $ "Closed connection on " ++ show s ++
                         " in " ++ show tid)


doMsgReception :: Service ss l => ss -> l -> IO ()
doMsgReception s l =
  orchRec $ mkScanData "" PPIn
  where
    orchRec sd' = do
      str <- ssRead s 16 l
      case str of
        RRData data' -> do let (msgs, sd'') = runScan (filter nonCRLF data') sd'
                           lWrite l $ "Read: [" ++ intercalate "|" msgs ++ "]"
                           orchRec sd''
        RRClosed -> lWrite l "Connection closed"
        RRError _ errMsg -> lWrite l $ "Error on connection: " ++ errMsg
    nonCRLF x = x /= '\n' && x /= '\r'


-------------------------------------------------------------------------------
--                                Instances
-------------------------------------------------------------------------------

instance Service Socket (BoundedChan String) where
  ssOpen l =
      bracketOnError
        (socket AF_INET Stream defaultProtocol)
        (\s -> ssClose s l)
        (\s -> do
            setSocketOption s ReuseAddr 1
            hostAddr <- inet_addr "127.0.0.1"
            lWrite l  $ "Socket " ++ show s ++ " opened"
            bind s (SockAddrInet 2222 hostAddr)
            listen s 5
            return s)
  ssClose s l = do close s
                   lWrite l $ "Socket closed " ++ show s
  ssAccept s _ = do (s', sa') <- accept s
                    return (s', show sa')

  ssRead s len _ = (RRData <$> recv s len) `catch` handler
    where
      handler :: IOException -> IO ReadRes
      handler e = case ioe_type e of
        EOF -> return RRClosed
        _ -> return $ RRError (fromInteger . toInteger <$> ioe_errno e) $ show e
