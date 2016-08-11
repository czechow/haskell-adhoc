module Thrds where

import GHC.Conc.Sync
import Control.Concurrent

-- Play with threads and examine events with ghc-events

main :: IO ()
main = do
  mTid <- myThreadId
  labelThread mTid "PCZ_MainThrd"
  tId <- forkIO $ longRunning
  labelThread tId "PCZ_Spawned1"
  threadDelay $ 1 * 1000 * 500
  --killThread tId
  --threadDelay $ 1 * 1000 * 1000


longRunning :: IO ()
longRunning = do
  threadDelay $ 1 * 1000 * 1000
