{-# LANGUAGE TemplateHaskell #-}

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

newtype RfqId = RfqId { unRfqId :: Integer }
              deriving Show

data RfqState = StateOpen
              | StateClosed
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

newtype StateSldWnd = StateSldWnd Integer
                    deriving Show

newtype StateTime = StateTime Time
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

processRules :: Maybe TimerTick -> Maybe Rfq -> State RulesState CbResult
processRules mTt mRfq = do
  let cbs1 = undefined <$> mTt
      cbs2 = undefined <$> mRfq
      cbs = catMaybes [cbs1, cbs2]
  return (cbs, [])
