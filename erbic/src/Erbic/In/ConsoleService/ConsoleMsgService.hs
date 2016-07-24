module Erbic.In.ConsoleService.ConsoleMsgService where

import Control.Concurrent hiding(writeList2Chan)
import Control.Concurrent.BoundedChan
import Control.Exception

import Erbic.Data.Msg.ScanMsg

runConsoleMsgService :: BoundedChan String -> IO (ThreadId, MVar ())
runConsoleMsgService ch = do
  mv <- newEmptyMVar :: IO (MVar ())
  tid <- forkIO $ processing mv
  return (tid, mv)
  where
    processing mv = do
      finally (orchRec $ mkScanData "" PPIn)
              (putMVar mv ())
    orchRec sd = do
      ln <- getLine
      case ln of
        "quit" -> return ()
        _ -> do let (msgs,  sd'') = runScan ln sd
                writeList2Chan ch msgs
                orchRec sd''
