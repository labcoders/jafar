module Types where

import Control.Monad.State
import Data.Time (UTCTime())

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

