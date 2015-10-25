{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Time (UTCTime())
import Data.Vector (Vector(), empty, singleton)

data Trend
    = UpTrend
    | DownTrend
        deriving (Eq, Show)

data Urgency
    = NotUrgent
    | Urgent
        deriving (Eq, Show)

data Outcome
    = Buy Position Urgency
    | Sell Position Urgency
    | DoNothing
        deriving (Eq, Show)

data Position
    = Long
    | Short
    | NoPosition
        deriving (Eq, Show)

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

data Value
    = Price Double
    | ValBool Bool
    | Time UTCTime
    | ValString String
    | Position Position
    | Trend Trend
        deriving (Eq, Show)

type Sensitivity = Double
type Price = Double
type Capital = Double

type Program = Action

data TimeInterval = TimeInterval
    { startTime :: UTCTime
    , endTime :: UTCTime
    }

timeInterval :: UTCTime -> UTCTime -> Maybe TimeInterval
timeInterval t1 t2
    | t1 < t2 = Just $ TimeInterval t1 t2
    | otherwise = Nothing

data EMA = EMA
    { emaShort :: Price
    , emaMedium :: Price
    , emaLong :: Price
    }

emaScore :: EMA -> Double
emaScore (EMA s m l)
    | cmp m s l = -1
    | cmp l s m = 1
    | cmp s m l = -5
    | cmp l m s = 5
    | cmp s l m = -7
    | cmp m l s = 7
    where cmp x y z = x <= y && y <= z

data Algorithm = Algorithm
    { algFreq :: Int -- ^ Number of seconds between nonurgent outcome realizations
    , algCode :: Program
    , algSensitivity :: Sensitivity
    }

data InitialCondition = InitialCondition
    { icStartingCapital :: Capital
    , icTimeInterval :: TimeInterval
    }

data BacktestDataset = BacktestDataset
    { bdData :: [(UTCTime, Price)]
    }

data JafarConf = JafarConf
    { jcSensitivity :: Sensitivity
    , jcTimeInterval :: TimeInterval
    , jcStartingCapital :: Capital
    , jcEMASizes :: (Int, Int, Int)
    , jcEMABacklog :: Int
    }

data JafarError = TypeError | NoOutcome
    deriving (Show)

data JafarState = JafarState
    { jsPrevStoploss :: Double
    , jsPosition :: Position
    , jsEMA :: Vector EMA
    , jsLastPrices :: Vector Price
    , jsCurrentTime :: UTCTime
    }

initialJafarState :: InitialCondition -> Price -> JafarState
initialJafarState ic p = JafarState
    { jsPrevStoploss = 0
    , jsPosition = NoPosition
    , jsEMA = singleton $ EMA p p p
    , jsLastPrices = empty
    , jsCurrentTime = startTime $ icTimeInterval ic
    }

newtype Jafar a = Jafar
    { runJafar :: ExceptT JafarError
                          (ReaderT JafarConf
                                   (StateT JafarState
                                           IO))
                  a
    }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError JafarError
             , MonadState JafarState
             , MonadReader JafarConf
             )

data BacktestResult = ResultSuccess | ResultFailure
    deriving (Show)
