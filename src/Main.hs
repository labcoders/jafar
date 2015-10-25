{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import qualified Data.Time as T
import Data.String
import Network.Blockchain
import System.Exit (exitFailure)

import Types
import Programs (emaProg)
import Interpreter

instance IsString T.UTCTime where
    fromString = fromMaybe (error "invalid time literal") . parseTime

parseTime = T.parseTimeM False T.defaultTimeLocale iso8601
    where iso8601 = T.iso8601DateFormat Nothing

main = do
    ps <- getChartData AllTime MarketPrice

    ps' <- case ps of
        Left e -> do
            putStrLn $ "error " ++ show e
            exitFailure
        Right ps -> return ps

    let a = Algorithm
                { algFreq = 10
                , algCode = emaProg
                , algSensitivity = 0.5
                }
        i = InitialCondition
                { icStartingCapital = 10000
                , icTimeInterval = fromMaybe (error "invalid time")
                                             (timeInterval "2015-10-01"
                                                           "2015-10-30")
                }
        d = BacktestDataset
                { bdData = map (\(ChartPoint x y) -> (btcTime x, y)) ps'
                }
    result <- backtest a i d
    print result
