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
import Data.Maybe
import Data.List
import Data.Ord
import qualified Text.Show.Pretty as Pr


newtype RfqId = RfqId { unRfqId :: Integer }
              deriving Show

data RfqState = RfqStateOpen
              | RfqStateClosed
              deriving Show

newtype RfqQuantity = RfqQuantity { unRfqQuantity :: Integer }
                    deriving Show

newtype RfqValue = RfqValue { unRfqValue :: Integer }
                 deriving Show

data Rfq = Rfq { _rfqId :: RfqId
               , _rfqState :: RfqState
               , _rfqQuantity :: RfqQuantity
               , _rfqValue :: RfqValue
               }
         deriving Show

makeRfq :: Integer -> RfqState -> Integer -> Integer -> Rfq
makeRfq rfqId' rfqState' rfqQuantity' rfqValue' =
  Rfq (RfqId rfqId') rfqState' (RfqQuantity rfqQuantity') (RfqValue rfqValue')

makeLenses ''Rfq

type Time = Integer

newtype TimerTick = TimerTick { unTick :: Time } -- as milliseconds...

-------------------------------------------------------------------------------
--                             Circuit breakers
-------------------------------------------------------------------------------

data CbState = Triggered
             | Armed
             deriving Show

data CbGen = CbGen { cbgState :: CbState, cbgDesc :: String }
           deriving Show

data CbRfq = CbRfq { cbrRfqId :: RfqId, cbrState :: CbState, cbrDesc :: String }
           deriving Show


-------------------------------------------------------------------------------
--                               Rule states
-------------------------------------------------------------------------------
data OperationMode = OmEnabled
                   | OmDisabled
                   deriving Show


-- FIXME: more advanced data structure here: like priority queue?
newtype StateRfq = StateRfq [Rfq]
                 deriving Show

newtype StateSldWnd = StateSldWnd [(Time, RfqId)]
                    deriving Show

newtype StateTime = StateTime (Maybe Time)
                    deriving Show
-------------------------------------------------------------------------------
--                               Main logic
-------------------------------------------------------------------------------
type CbResult = ([CbGen], [CbRfq])

data RulesState = RulesState { _sTime :: StateTime
                             , _sRfq :: StateRfq
                             , _sSldWnd :: StateSldWnd
                             }
                deriving Show

makeLenses ''RulesState

initialState :: RulesState
initialState = RulesState { _sTime = StateTime Nothing
                          , _sRfq = StateRfq []
                          , _sSldWnd = StateSldWnd []
                          }

runRule :: i -> (i -> s -> (o, s)) -> Lens' RulesState s -> State RulesState o
runRule inputs fS l = do
  st <- get
  let s = view l st
      (o, s') = fS inputs s
  put $ set l s' st
  return $ o


-- Write is as easily as you can
-- Monads will come later



-- update time info, that's all
-- perhaps truncating an RFQ queue?
-- validations: FIXME: disallow running timer in reverse order
processTimerTick :: Maybe TimerTick -> State RulesState ()
processTimerTick Nothing = return ()
processTimerTick (Just tick) = do
  runRule tick updateTick sTime
    where
      updateTick :: TimerTick -> StateTime -> ((), StateTime)
      updateTick tick' _ = ((), StateTime $ Just $ unTick tick')

processRfq :: Maybe Rfq -> State RulesState [CbGen]
processRfq Nothing = return []
processRfq (Just rfq) = do
  rfqGcbs <- runRule rfq addRfq sRfq
  st <- get
  sldWndGcbs <- runRule (_rfqId rfq, view sTime st) calcSldWnd sSldWnd
  return $ rfqGcbs ++ sldWndGcbs

-- is it ok? Yes, for the time being...
addRfq :: Rfq -> StateRfq -> ([CbGen], StateRfq)
addRfq rfq (StateRfq s) = ([], StateRfq $ rfq : s)

calcSldWnd :: (RfqId, StateTime) -> StateSldWnd -> ([CbGen], StateSldWnd)
calcSldWnd (_, StateTime Nothing) _ = error("timer not set, fail...")
calcSldWnd (rfqId', StateTime (Just t)) (StateSldWnd ws) =
  (cbs, StateSldWnd ws')
  where
    ws' = dropWhile ((>15) . (flip subtract t) . fst) $
          sortBy (comparing fst) $ (t, rfqId') : ws
    cbs = if (length $ take 3 ws') == 3
          then [CbGen Triggered "Too many RFQs (XX) in YY secs"]
          else []


processRules :: (Maybe TimerTick, Maybe Rfq) -> State RulesState CbResult
processRules (mTt, mRfq) = do
  processTimerTick mTt
  rfqGcbs <- processRfq mRfq
  return (rfqGcbs, []) -- some kind of union, I guess...



inputData :: [(Maybe TimerTick, Maybe Rfq)]
inputData = [ (Just $ TimerTick 10, Nothing)
            , (Nothing, Just $ makeRfq 1 RfqStateOpen 13 997)
            , (Just $ TimerTick 11, Nothing)
            , (Nothing, Just $ makeRfq 2 RfqStateOpen 14 998)
            , (Just $ TimerTick 25, Nothing)
            , (Nothing, Just $ makeRfq 3 RfqStateOpen 15 999)
            , (Just $ TimerTick 35, Nothing)
            , (Nothing, Just $ makeRfq 4 RfqStateOpen 16 1000)
            ]

initialOutput :: CbResult
initialOutput = ([], [])

go :: IO ()
go = do
  putStrLn $ "Initial state: [" ++ show initialState ++ "]"
  let allRes = scanl (\(_, st) inputs -> runState (processRules inputs) st)
                     (initialOutput, initialState) inputData
  putStrLn $ "Result after all calcs:"
  putStrLn $ Pr.ppShow allRes
