module TVars where

import Control.Concurrent.STM
import Data.Map.Strict
import System.IO.Unsafe
import Control.Concurrent
import Control.Monad

book :: TVar (Map String String)
book = unsafePerformIO $ newTVarIO empty

addEntrySTM :: String -> String -> STM ()
addEntrySTM k v = modifyTVar book $ insert k v

main :: IO ()
main = do
  putStrLn "Up and running"
  atomically $ do
    addEntrySTM "Bob" "X"
    addEntrySTM "Alice" "Y"
  putStrLn $ "Press enter to quit"
  tid <- forkIO $ dumpBookLoop
  void getLine
  killThread tid



dumpBookLoop :: IO ()
dumpBookLoop = do
  dumpBook
  threadDelay $ 1 * 1000 * 1000
  dumpBookLoop

dumpBook :: IO ()
dumpBook = do
  z <- atomically $ readTVar book
  putStrLn $ "Book: " ++ show z
