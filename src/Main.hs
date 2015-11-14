{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import qualified Data.Time as T
import Data.String
import qualified Network.Blockchain as B
import System.Exit (exitFailure)

import Types
import Programs (emaProg)
import Interpreter

instance IsString T.UTCTime where
    fromString = fromMaybe (error "invalid time literal") . parseTime

parseTime = T.parseTimeM False T.defaultTimeLocale iso8601
    where iso8601 = T.iso8601DateFormat Nothing

main = do
    ps <- B.getChartData B.Years1 B.MarketPrice

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
                { bdData = map (\(B.ChartPoint x y) -> (B.btcTime x, y)) ps'
                }

    result <- backtest a i d
    let ts = jsTransactions result
    forM_ ts $ \(Transaction tx pos amt curr price time) -> do
        putStrLn $ intercalate "," $ case (tx, curr) of
            (TxBuy, BTC) -> [ show time
                            , show tx
                            , show pos
                            , show amt ++ " BTC"
                            , show (amt*price) ++ " USD"
                            , show price
                            ]
            (TxSell, BTC) -> [ show time
                             , show tx
                             , show pos
                             , show amt ++ " BTC"
                             , show (amt*price) ++ " USD"
                             , show price
                             ]
            (TxBuy, USD) -> [ show time
                            , show tx
                            , show pos
                            , show (amt/price) ++ " BTC"
                            , show amt ++ " USD"
                            , show price
                            ]
            (TxSell, USD) -> [ show time
                             , show tx
                             , show pos
                             , show (amt/price) ++ " BTC"
                             , show amt ++ " USD"
                             , show price
                             ]
        
