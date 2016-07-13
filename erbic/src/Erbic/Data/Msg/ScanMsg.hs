module Erbic.Data.Msg.ScanMsg
       ( mkScanData
       , runScan
       , ScanData(ScanData)
       , PacketParse(PPIn, PPOut)
       , sep
       , maxBuffLen
       ) where


import Data.List.Split (splitOn)
import Control.Monad.State


type Buffer = String
type Msg = String

data PacketParse = PPIn | PPOut
                 deriving Show

data ScanData = ScanData { leftOver :: Buffer
                         , parseState :: PacketParse
                         , overrunCnt :: Int
                         , syncCnt :: Int }
              deriving Show


maxBuffLen :: Int
maxBuffLen = 23

sep :: String
sep = "-"


mkScanData :: Buffer -> PacketParse -> ScanData
mkScanData buff pp = ScanData buff pp 0 0

scanForMsg :: String -> State ScanData [Msg]
scanForMsg xs = do
  sd@(ScanData buff pp oCnt sCnt) <- get
  let buff' = buff ++ xs
      drpCnt = length buff' - maxBuffLen
  if drpCnt > 0
    then do put sd { leftOver = (drop drpCnt buff')
                   , parseState = PPOut
                   , overrunCnt = (succ oCnt) }
            scanForMsg []
    else case (splitOn sep buff', pp) of
      (a@(_ : _ : _), PPIn) -> do put sd { leftOver = last a }
                                  return $ init a
      (a@(_ : _ : _), PPOut) -> do put sd { leftOver = last a
                                          , parseState = PPIn
                                          , syncCnt = succ sCnt }
                                   return $ init $ tail a
      (m : _, PPOut) -> do put sd { leftOver = drop (length m) buff' }
                           return []
      (_, _) -> do put sd { leftOver = buff' }
                   return []

-- FIXME: do we really need this?
runScan :: String -> ScanData -> ([Msg], ScanData)
runScan xs s = runState (scanForMsg xs) s
