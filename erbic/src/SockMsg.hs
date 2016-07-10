{-# LANGUAGE FlexibleInstances #-}

module SockMsg where


import Control.Concurrent.BoundedChan
import Data.List (intercalate)

import Sock
import ScanMsg


-- FIXME: perhaps the name should be more generic???

class ChannelService a where
  csWrite :: a -> [String] -> IO ()

instance ChannelService (BoundedChan String) where
  csWrite ch msgs = writeList2Chan ch msgs

-- FIXME: should inform on result (if connection is closed/broken)
orchMsgReception :: (SockService ss, ChannelService cs)  => ss -> cs -> IO ()
orchMsgReception s ch =
  orchRec $ mkScanData "" PPIn
  where
    orchRec sd' = do
      str <- ssRead s 16
      case str of
        RRData data' -> do let (msgs, sd'') = runScan (filter nonCRLF data') sd'
                           putStrLn $ "Read: [" ++ intercalate "|" msgs ++ "]"
                           csWrite ch msgs
                           orchRec sd''
        RRClosed -> putStrLn "Connection closed"
        RRError _ errMsg -> putStrLn $ "Error on connection: " ++ errMsg
    nonCRLF x = x /= '\n' && x /= '\r'
