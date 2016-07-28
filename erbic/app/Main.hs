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
  ch <- newBoundedChan 128 :: IO (BoundedChan String)
  ssd <- SMS.newSS "X" 12 ch
         :: IO (SMS.SSInitData Socket (BoundedChan String))
  bracket (tfork $ logger ch) (stopLogger ch) $ \_ ->
    bracket (SMS.startSS ssd) (\ssd' -> SMS.stopSS 0 ssd') $ \_ ->
    bracket (runConsoleMsgService ch) (stopConsoleMsgService) $ \(cstid, mv) ->
    do
      putStrLn "Enter <quit> to stop"
      _ <- readMVar mv
      putStrLn $ "Console thread stopped " ++ show cstid
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

-- make sure we spit out all the messages
stopLogger :: BoundedChan String -> ThreadInfo -> IO ()
stopLogger ch ti = do
  stopThread 0 ti
  msgs <- rdChan
  mapM_ (\m -> putStrLn $ "[LOG flush]: " ++ m) msgs
  where
    rdChan :: IO [String]
    rdChan = do
      mMsg <- tryReadChan ch
      case mMsg of
        Just msg -> (msg :) <$> rdChan
        Nothing -> return []
