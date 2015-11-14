{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Applicative hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans
import Data.Time (UTCTime())
import Data.Vector (Vector(), empty, singleton)

-- | Enumeration type for the different currencies available to the trader.
data Currency = BTC | USD
    deriving (Show)

-- | Represents the funds held by the trader and the price of Bitcoin at the
-- time those funds were acquired.
data Funds = Funds Currency Double Price
    deriving (Show)

-- | Represents a trend in the market price of Bitcoin.
data Trend
    = UpTrend
    | DownTrend
        deriving (Eq, Show)

-- | Urgent trades are executed even if they occur too soon after another
-- trade. The main use for this is to execute so-called "stoploss" trades when
-- the market is determined to be performing unexpectedly poorly.
data Urgency
    = NotUrgent
    | Urgent
        deriving (Eq, Show)

-- | Represents the signal produced by running the trade decision procedure.
data Outcome
    = Buy Position Urgency
    | Sell Position Urgency
    | DoNothing
        deriving (Eq, Show)

-- | Represents a position held by the trader in the market.
data Position
    = Long
    | Short
    | NoPosition
        deriving (Eq, Show)

-- | Syntax tree for a trade decision procedure.
data Action
    = GetTrend
    | GetCrossover
    | GetPosition
    | GetStopLoss
    | GetPrice
    | Emit Outcome
    | If Action Action Action
    | Compare Ordering Action Action
    | Lit Value
    | Not Action
        deriving (Eq, Show)

-- | Different values available to the trade decision procedure.
data Value
    = Price Double
    | ValBool Bool
    | Time UTCTime
    | ValString String
    | Position Position
    | Trend Trend
        deriving (Eq, Show)

-- | The sensitivity of the exponential moving average curve calculator.
type Sensitivity = Double

-- | A price is simply represented as a "Double".
type Price = Double

-- | An amount of capital is simply represented as a "Double".
type Capital = Double

-- | A trade decision procedure is a program represented by a syntax tree.
type Program = Action

-- | A span of time represented by a start point and an end point.
data TimeInterval
    -- | A time interval for backtesting has a fixed start and end time, both
    -- of them in the past.
    = TimeInterval
        { startTime :: UTCTime
        , endTime :: UTCTime
        }

-- | Safely constructs a time interval by checking that the start time is
-- before the end time.
timeInterval :: UTCTime -- ^ start time
             -> UTCTime -- ^ end time
             -> Maybe TimeInterval
timeInterval t1 t2
    | t1 < t2 = Just $ TimeInterval t1 t2
    | otherwise = Nothing

-- | Three exponential moving averages of the market price of Bitcoin represent
-- immediate market trends at some time. The short term EMA must be averaged
-- over a smaller dataset than the medium term EMA, and the medium term EMA
-- must be averaged over a smaller dataset than the long term EMA.
data EMA = EMA
    { emaShort :: Price
    , emaMedium :: Price
    , emaLong :: Price
    }
        deriving (Show)

-- | Give a trend score to an EMA triple.
emaScore :: EMA -> Double
emaScore (EMA s m l)
    | cmp m s l = -1
    | cmp l s m = 1
    | cmp s m l = -5
    | cmp l m s = 5
    | cmp s l m = -7
    | cmp m l s = 7
    where cmp x y z = x <= y && y <= z

-- | A trade decision algorithm is a trade decision procedure run at some
-- frequency with some sensitivity.
data Algorithm = Algorithm
    { algFreq :: Int
    -- ^ Number of seconds between nonurgent outcome realizations at least
    , algCode :: Program
    -- ^ The code of the trade decision procedure.
    , algSensitivity :: Sensitivity
    -- ^ The sensitivity of the EMA calculations.
    }

-- | Starting conditions for a trade decision algorithm.
data InitialCondition = InitialCondition
    { icStartingCapital :: Capital
    -- ^ The starting capital of the trader.
    , icTimeInterval :: TimeInterval
    -- ^ The backtesting time interval.
    }

-- | A dataset for backtesting is simply a list of timestamped prices,
-- sorted chronologically.
data BacktestDataset = BacktestDataset
    { bdData :: [(UTCTime, Price)]
    }

-- | A transaction type is either to buy or to sell.
data Tx = TxBuy | TxSell
    deriving (Show)

data Transaction = Transaction
    { txType :: Tx
    , txPosition :: Position
    , txAmount :: Double
    , txCurrency :: Currency
    , txActivePrice :: Price
    , txTime :: UTCTime
    }
    deriving (Show)

txValue :: Transaction -> Currency -> Double
txValue tx c' =
    case (c, c') of
        (BTC, BTC) -> amt
        (USD, USD) -> amt
        (USD, BTC) -> amt / price
        (BTC, USD) -> amt * price
    where c = txCurrency tx
          amt = txAmount tx
          price = txActivePrice tx

-- | The configuration of a Jafar backtester.
--
-- This is an aggregate of some of the data held in an "Algorithm" and an
-- "InitialCondition".
data JafarConf = JafarConf
    { jcSensitivity :: Sensitivity
    -- ^ The sensitivity of the EMA calculations.
    , jcTimeInterval :: TimeInterval
    -- ^ The backtesting time interval.
    , jcStartingCapital :: Capital
    -- ^ The starting capital of the trader.
    , jcEMASizes :: (Int, Int, Int)
    -- ^ The number of data points to use for the short, medium, and long EMA
    -- calculations, respectively.
    , jcEMABacklog :: Int
    -- ^ The number of exponential moving average triples to hold in a backlog
    -- used to determine longer-term market trends.
    }

-- | Different runtime errors that can arise from executing a "Program".
data JafarError
    = TypeError
    -- ^ A type mismatch occurs.
    | NoOutcome
    -- ^ The "Program" does not evaluate to an "Outcome".
    | IllegalOperation
    -- ^ Unused.
    | WrongFunds
    -- ^ A signal to buy some currency is issued when the current funds are
    -- already in that currency, or a signal to sell some currency is issued
    -- when no such currency is presently held.
    | NoPrice
    -- ^ Unused.
    deriving (Show)

-- | The mutable state of the backtester.
data JafarState = JafarState
    { jsPrevStoploss :: !Double
    -- ^ The last used value to determine whether the trader should cut its
    -- losses.
    , jsPosition :: !Position
    -- ^ The current position of the trader.
    , jsEMA :: !(Vector EMA)
    -- ^ The backlog of EMA curves used to determine longer-term market trends.
    -- The length of this vector is at most "jcEMABacklog" in the "Jafar"
    -- monad.
    , jsLastPrices :: !(Vector Price)
    -- ^ A backlog of Bitcoin market prices used to compute the exponential
    -- moving averages.
    , jsCurrentTime :: !UTCTime
    -- ^ The current time from the perspective of the next execution of the
    -- trade decision procedure.
    , jsFunds :: !Funds
    -- ^ The funds held by the trader.
    , jsTransactions :: ![Transaction]
    -- ^ A history of executed transactions.
    }
    deriving (Show)

-- | Builds an initial "JafarState" given an "InitialCondition" and a starting
-- "Price" of Bitcoin.
initialJafarState :: InitialCondition -> Price -> JafarState
initialJafarState ic p = JafarState
    { jsPrevStoploss = 0
    , jsPosition = NoPosition
    , jsEMA = singleton $ EMA p p p
    , jsLastPrices = empty
    , jsCurrentTime = startTime $ icTimeInterval ic
    , jsFunds = Funds USD (icStartingCapital ic) 0
    , jsTransactions = []
    }

-- | The Jafar monad is capable of throwing a "JafarError", reading from a
-- "JafarConf", and mutating a "JafarState" over "IO".
newtype Jafar a = Jafar
    { runJafar :: ExceptT JafarError
                          (ReaderT JafarConf
                                   (StateT JafarState
                                           IO))
                  a
    -- ^ Unwraps a Jafar computation into its constituent monad transformers.
    }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError JafarError
             , MonadState JafarState
             , MonadReader JafarConf
             , MonadIO
             )

-- | The result of a backtest.
data BacktestResult = ResultSuccess | ResultFailure
    deriving (Show)
