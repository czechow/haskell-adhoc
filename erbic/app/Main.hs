{-# LANGUAGE LambdaCase #-}

module Main where


import Control.Monad.State
import Control.Concurrent.BoundedChan
import Control.Concurrent hiding (writeChan, readChan, getChanContents)
import Control.Exception


import Network.Socket
import Erbic.IO.Fork
import qualified Erbic.In.SockService.SockMsgService2 as SMS
import Erbic.In.ConsoleService.ConsoleMsgService


main :: IO ()
main = do
  putStrLn "Main thread started"
  ch <- newBoundedChan 128
  ssd <- SMS.makeSSInitData "X" 12 ch
         :: IO (SMS.SSInitData Socket (BoundedChan String))
  -- ssd <- SMS.makeSSInitData "X" 12 (SMS.IgnoringLogger ())
  --        :: IO (SMS.SSInitData Socket SMS.IgnoringLogger)

  bracket (tfork $ logger ch) (stopLogger ch) $ \_ ->
    bracket (SMS.startSS ssd) (\ssd' -> SMS.stopSS ssd' 0) $ \_ ->
    bracket (runConsoleMsgService ch) (stopConsoleMsgService) $ \(_, mv) ->
    do
      putStrLn "Enter <quit> to stop"
      _ <- readMVar mv
      return ()
  putStrLn $ "Main thread stopped"

logger :: BoundedChan String -> IO ()
logger ch = forever $ mask_ $ do
  msg <- readChan ch
  putStrLn $ "[LOG]: " ++ msg

stopConsoleMsgService :: (ThreadId, MVar ()) -> IO ()
stopConsoleMsgService (tid, mv) = do
  putStrLn $ "Stopping console service " ++ show tid
  killThread tid
  readMVar mv
  putStrLn $ "Console service stopped " ++ show tid


stopLogger :: BoundedChan String -> ThreadInfo -> IO ()
stopLogger ch ti = do
  stopThread 0 ti
  -- Make sure we spit out all the messages
  msgs <- rdChan
  mapM_ (\m -> putStrLn $ "[LOG flush]: " ++ m) msgs
  where
    rdChan = tryReadChan ch >>= \case Just msg -> (msg :) <$> rdChan
                                      Nothing -> return []
