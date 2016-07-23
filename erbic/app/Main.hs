module Main where


import Control.Monad.State
import Control.Concurrent.BoundedChan
import Control.Concurrent hiding (readChan, getChanContents)
import Control.Exception


import Network.Socket
import Erbic.IO.Fork
import qualified Erbic.In.SockService.SockMsgService2 as SMS
import Erbic.In.ConsoleService.ConsoleMsgService

delay :: Int -> IO a -> IO a
delay us action = do
  threadDelay us
  action

main :: IO ()
main = do
  putStrLn "Up and running"
  ch <- newBoundedChan 128 :: IO (BoundedChan String)
  ssd <- SMS.newSS "X" 12 ch :: IO (SMS.SSData Socket)
  bracket (tfork $ logger ch) (stopLogger ch) $ \_ ->
    bracket (SMS.startSS ssd) (\ssd' -> SMS.stopSS 0 ssd') $ \_ ->
    bracket (runConsoleMsgService ch) (stopConsoleMsgService) $ \(cstid, mv) ->
    do
      putStrLn "Enter <quit> to stop"
      _ <- readMVar mv
      putStrLn $ "Console thread stopped " ++ show cstid
      return ()


logger :: BoundedChan String -> IO ()
logger ch = forever $ do
  msg <- readChan ch
  putStrLn $ "[LOG]: " ++ msg

stopConsoleMsgService :: (ThreadId, MVar ()) -> IO ()
stopConsoleMsgService (tid, _) = do
  putStrLn $ "Stopping console service " ++ show tid
  killThread tid

-- make sure we spit out all the messages
stopLogger :: BoundedChan String -> ThreadInfo -> IO ()
stopLogger ch ti = do
  putStrLn $ "Stopping logger thread " ++ (show $ fst ti)
  stopThread 0 ti
  putStrLn $ "Logger thread stopped"
  putStrLn $ "Flushing logger queue"
  msgs <- getChanContents ch
  mapM_ (\m -> putStrLn $ "[LOG]: Flush: " ++ m) msgs
  putStrLn "Logger queue flushed"
