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
import Data.List
import Data.Ord
import qualified Text.Show.Pretty as Pr


newtype RfqId = RfqId { unRfqId :: Integer }
              deriving (Show, Eq)

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


-- FIXME: more advanced data structure here: like priority queue?
newtype StateRfq = StateRfq [Rfq]
                 deriving (Show, Eq)

newtype StateSldWnd = StateSldWnd [(Time, RfqId)]
                    deriving (Show, Eq)

newtype StateTime = StateTime Time
                     deriving (Show, Eq)

type StateParams = Params
-------------------------------------------------------------------------------
--                               Main logic
-------------------------------------------------------------------------------
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
                          , _sRfq = StateRfq []
                          , _sSldWnd = StateSldWnd []
                          , _sParams = makeParams 15 3
                          }

runRule :: i -> (i -> s -> (o, s)) -> Lens' RulesState s -> State RulesState o
runRule inputs fS l = do
  st <- get
  let s = view l st
      (o, s') = fS inputs s
  put $ set l s' st
  return $ o


processTimerTick :: Maybe TimerTick -> State RulesState CbResult
processTimerTick Nothing = return []
processTimerTick (Just tick) = do
  runRule tick updateTick sTime
    where
      updateTick :: TimerTick -> StateTime -> (CbResult, StateTime)
      updateTick (TimerTick t) (StateTime prevT)
        | prevT <= t = ([], StateTime t)
        | otherwise  = ([CbImpl CbiWarn $
                         "Ignoring new time tick of [" ++ show t ++
                         "] because it is in the past (current: [" ++
                         show prevT ++ "])"]
                       , StateTime prevT)

processParams :: Maybe Params -> State RulesState CbResult
processParams Nothing = return []
processParams (Just params) = do
  _ <- runRule params (\ps _ -> ((), ps)) sParams
  st <- get
  runRule (Nothing,
           view sTime st,
           view (sParams . sldWndTime) st,
           view (sParams . sldWndCnt) st)
          calcSldWnd sSldWnd

processRfq :: Maybe Rfq -> State RulesState [Cb]
processRfq Nothing = return []
processRfq (Just rfq) = do
  rfqGcbs <- runRule rfq addRfq sRfq
  st <- get
  sldWndGcbs <- runRule (Just $ view rfqId rfq,
                         view sTime st,
                         view (sParams . sldWndTime) st,
                         view (sParams . sldWndCnt) st)
                        calcSldWnd sSldWnd
  return $ rfqGcbs ++ sldWndGcbs

-- is it ok? Yes, for the time being...
addRfq :: Rfq -> StateRfq -> ([Cb], StateRfq)
addRfq rfq (StateRfq s) = ([], StateRfq $ rfq : s)

calcSldWnd :: (Maybe RfqId, StateTime, SldWndTime, SldWndCnt)
           -> StateSldWnd
           -> ([Cb], StateSldWnd)
calcSldWnd (mRfqId', StateTime t, SldWndTime swt, SldWndCnt swc)
           (StateSldWnd ws) =
  case mRfqId' of
    Just rfqId' -> f (calcWs $ (t, rfqId') : ws)
    Nothing -> f (calcWs ws)
  where
    f = (,) <$> calcCbs <*> StateSldWnd -- AppFunct Reader instance
    calcWs ws' = dropWhile ((>swt) . (flip subtract t) . fst) $
                           sortBy (comparing fst) $ ws' -- sort avoided if pq
    calcCbs ws' = if (length $ take swc $ ws') == swc
                  then [CbGen Triggered $
                        "Too many RFQs (limit is " ++ show swc ++ ") in " ++
                        show swt ++ " secs"]
                  else []


processRules :: (Maybe TimerTick, Maybe Params, Maybe Rfq)
             -> State RulesState [Cb]
processRules (mTt, mParams, mRfq) =
  concat <$> sequence [ processTimerTick mTt
                      , processParams mParams
                      , processRfq mRfq ]


inputData :: [(Maybe TimerTick, Maybe Params, Maybe Rfq)]
inputData = [ (Just $ TimerTick 10, Nothing, Nothing)
            , (Nothing, Nothing, Just $ makeRfq 1 RfqStateOpen 13 997)
            , (Just $ TimerTick 11, Nothing, Nothing)
            , (Nothing, Nothing, Just $ makeRfq 2 RfqStateOpen 14 998)
            , (Just $ TimerTick 25, Nothing, Nothing)
            , (Nothing, Nothing, Just $ makeRfq 3 RfqStateOpen 15 999)
            , (Just $ TimerTick 35, Just $ makeParams 15 2, Nothing)
            , (Nothing, Nothing, Just $ makeRfq 4 RfqStateOpen 16 1000)
            , (Nothing, Just $ makeParams 15 3, Nothing)
            , (Nothing, Just $ makeParams 15 1, Nothing)
            , (Just $ TimerTick 34, Nothing, Nothing)
            ]

initialOutput :: CbResult
initialOutput = []

go :: IO ()
go = do
  putStrLn $ "Initial state: [" ++ show initialState ++ "]"
  let allRes = scanl (\(_, st) inputs -> runState (processRules inputs) st)
                     (initialOutput, initialState) inputData
  putStrLn $ "Result after all steps:"
  putStrLn $ Pr.ppShow allRes
