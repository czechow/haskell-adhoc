{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Graph
       -- ( RfqId
       -- , unRfqId
       -- , RfqState
       -- , RfqQuantity
       -- , unRfqQuantity
       -- , RfqValue
       -- , unRfqValue
       -- , Rfq
       -- , rfqId
       -- , rfqState
       -- , rfqQuantity
       -- , rfqValue
       -- )
       where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Text.Show.Pretty as Pr
import qualified Data.OrdPSQ as PQ

newtype RfqId = RfqId { unRfqId :: Integer }
              deriving (Show, Eq, Ord)

data RfqState = RfqStateOpen
              | RfqStateClosed
              deriving (Show, Eq)

newtype RfqQuantity = RfqQuantity { unRfqQuantity :: Integer }
                    deriving (Show, Eq)

newtype RfqValue = RfqValue { unRfqValue :: Integer }
                 deriving (Show, Eq)

data Rfq = Rfq { _rfqId :: RfqId
               , _rfqState :: RfqState
               , _rfqQuantity :: RfqQuantity
               , _rfqValue :: RfqValue
               }
         deriving (Show, Eq)

makeRfq :: Integer -> RfqState -> Integer -> Integer -> Rfq
makeRfq rfqId' rfqState' rfqQuantity' rfqValue' =
  Rfq (RfqId rfqId') rfqState' (RfqQuantity rfqQuantity') (RfqValue rfqValue')

makeLenses ''Rfq

type Time = Integer

newtype TimerTick = TimerTick { unTick :: Time } -- as milliseconds...
                  deriving (Show)

type TimeSpan = Integer

newtype SldWndTime = SldWndTime Time
                    deriving (Show, Eq)

newtype SldWndCnt = SldWndCnt Int
                  deriving (Show, Eq)

data Params = Params { _sldWndTime :: SldWndTime
                     , _sldWndCnt :: SldWndCnt
                     }
            deriving (Show, Eq)

makeParams :: Time -> Int -> Params
makeParams t c = Params (SldWndTime t) (SldWndCnt c)

makeLenses ''Params

-------------------------------------------------------------------------------
--                             Circuit breakers
-------------------------------------------------------------------------------

data CbState = Triggered
             | Armed
             deriving (Show, Eq)

data CbiLevel = CbiWarn
              | CbiError
              | CbiInfo
              deriving (Show, Eq)

data Cb = CbGen { cbgState :: CbState, cbgDesc :: String }
        | CbRfq { cbrRfqId :: RfqId, cbrState :: CbState, cbrDesc :: String }
        | CbImpl { cbiLevel :: CbiLevel,  cbiDescr :: String }
        deriving (Show, Eq)

-------------------------------------------------------------------------------
--                               Rule states
-------------------------------------------------------------------------------
data OperationMode = OmEnabled
                   | OmDisabled
                   deriving Show


newtype StateRfq = StateRfq (Map.Map RfqId Rfq)
                 deriving (Show, Eq)

newtype StateSldWnd = StateSldWnd (PQ.OrdPSQ RfqId Time ())
                    deriving (Show, Eq)

newtype StateTime = StateTime { unStateTime :: Time }
                  deriving (Show, Eq)

type StateParams = Params
-------------------------------------------------------------------------------
--                               Main logic
-------------------------------------------------------------------------------
data InputMessage = ImTimer TimerTick
                  | ImParams Params
                  | ImRfq Rfq
                  | ImDelRfq RfqId
                  deriving Show

type CbResult = [Cb]

data RulesState = RulesState { _sTime :: StateTime
                             , _sRfq :: StateRfq
                             , _sSldWnd :: StateSldWnd
                             , _sParams :: StateParams
                             }
                deriving (Show, Eq)

makeLenses ''RulesState


initialState :: RulesState
initialState = RulesState { _sTime = StateTime 0
                          , _sRfq = StateRfq Map.empty
                          , _sSldWnd = StateSldWnd PQ.empty
                          , _sParams = makeParams 15 3
                          }


runRule :: i -> (i -> s -> (o, s))
        -> Lens' RulesState s
        -> ProcMonad [IntCmd] RulesState o
runRule inputs fS l = do
  st <- get
  let s = view l st
      (o, s') = fS inputs s
  put $ set l s' st
  return $ o

type ProcMonad w s r = WriterT w (State s) r

data IntCmd = IcDelRfq Time RfqId
            deriving Show


processTimerTick :: TimerTick -> ProcMonad [IntCmd] RulesState CbResult
processTimerTick tick = do
  runRule tick updateTick sTime
    where
      updateTick :: TimerTick -> StateTime -> (CbResult, StateTime)
      updateTick (TimerTick t) (StateTime prevT)
        | prevT <= t = ([], StateTime t)
        | otherwise  = ([ CbImpl CbiWarn $
                          "Ignoring new time tick of [" ++ show t ++
                          "] because it is in the past (current: [" ++
                          show prevT ++ "])" ]
                       , StateTime prevT)


processParams :: Params -> ProcMonad [IntCmd] RulesState CbResult
processParams params = do
  _ <- runRule params (\ps _ -> ((), ps)) sParams
  st <- get
  runRule (Nothing,
           view sTime st,
           view (sParams . sldWndTime) st,
           view (sParams . sldWndCnt) st)
          calcSldWnd sSldWnd


processRfq :: Rfq -> ProcMonad [IntCmd] RulesState CbResult
processRfq rfq = do
  rfqGcbs <- runRule rfq addRfq sRfq
  st <- get
  tell [IcDelRfq ((unStateTime $ view sTime st) + 30) (view rfqId rfq)] -- FIXME: 30
  sldWndGcbs <- runRule (Just $ view rfqId rfq,
                         view sTime st,
                         view (sParams . sldWndTime) st,
                         view (sParams . sldWndCnt) st)
                        calcSldWnd sSldWnd
  return $ rfqGcbs ++ sldWndGcbs


addRfq :: Rfq -> StateRfq -> ([Cb], StateRfq)
addRfq rfq (StateRfq m) = ([], StateRfq $ Map.insert (view rfqId rfq) rfq m)


calcSldWnd :: (Maybe RfqId, StateTime, SldWndTime, SldWndCnt)
           -> StateSldWnd
           -> ([Cb], StateSldWnd)
calcSldWnd (mRfqId', StateTime t, SldWndTime swt, SldWndCnt swc)
           (StateSldWnd m) =
  case mRfqId' of
    Just rfqId' -> f (sldWnd $ PQ.insert rfqId' t () m)
    Nothing -> f (sldWnd m)
  where
    f = (,) <$> cbs <*> StateSldWnd
    sldWnd m' = pqDropWhile (\_ te _ -> t - te > 15) m'
    cbs m' = if PQ.size m' >= swc
             then [CbGen Triggered $
                   "Number of RFQs " ++ show (PQ.size m') ++ " in the last " ++
                   show swt ++ " seconds exceeds the limit of " ++ show swc ]
             else []


pqDropWhile :: (Ord k, Ord p) => (k -> p -> v -> Bool)
            -> PQ.OrdPSQ k p v
            -> PQ.OrdPSQ k p v
pqDropWhile pred' pq = case PQ.findMin pq of
  Nothing -> pq
  Just (k, p, v) -> if (pred' k p v)
                    then pqDropWhile pred' $ PQ.deleteMin pq
                    else pq


processDelRfq :: RfqId -> ProcMonad [IntCmd] RulesState CbResult
processDelRfq rfqId' = do
  liftM concat $ sequence [ runRule rfqId' delRfq sRfq
                          , runRule rfqId' delRfq2 sSldWnd ]
    where
      delRfq :: RfqId -> StateRfq -> ([Cb], StateRfq)
      delRfq rfqId'' (StateRfq m) = ([], StateRfq $ Map.delete rfqId'' m)
      delRfq2 :: RfqId -> StateSldWnd -> ([Cb], StateSldWnd)
      delRfq2 rfqId'' (StateSldWnd m) = ([], StateSldWnd $ PQ.delete rfqId'' m)



processRules :: InputMessage -> ProcMonad [IntCmd] RulesState CbResult
processRules (ImTimer tt) = processTimerTick tt
processRules (ImParams params) = processParams params
processRules (ImRfq rfq) = processRfq rfq
processRules (ImDelRfq rfqId') = processDelRfq rfqId'


inputData :: [InputMessage]
inputData = [ ImTimer $ TimerTick 10
            , ImRfq $ makeRfq 1 RfqStateOpen 13 997
            , ImTimer $ TimerTick 11
            , ImRfq $ makeRfq 2 RfqStateOpen 14 998
            , ImTimer $ TimerTick 25
            , ImRfq $ makeRfq 3 RfqStateOpen 15 999
            , ImTimer $ TimerTick 35
            , ImParams $ makeParams 15 2
            , ImRfq $ makeRfq 4 RfqStateOpen 16 1000
            , ImParams $ makeParams 15 3
            , ImParams $ makeParams 15 1
            , ImTimer $ TimerTick 34
            , ImDelRfq $ RfqId 4
            ]


initialOutput :: CbResult
initialOutput = []

initialCmds :: [IntCmd]
initialCmds = []

go :: IO ()
go = do
  putStrLn $ "Initial state: [" ++ show initialState ++ "]"
  let allRes = scanl (\((_, cmds), st) inputs ->
                       runState (runWriterT $ processRules inputs) st)
                      ((initialOutput, initialCmds), initialState) inputData
  putStrLn $ "Result after all steps:"
  putStrLn $ Pr.ppShow allRes


newtype IntCmdQueueKey = IcqKey Int -- make it unique
type IntCmdQueue = PQ.OrdPSQ IntCmdQueueKey Time ()


runProc :: InputMessage -> RulesState -> [((CbResult, [IntCmd]), RulesState)]
runProc msg st = undefined
