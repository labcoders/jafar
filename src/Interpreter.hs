module Interpreter where

import Types
import Sugar

import Control.Applicative hiding (empty)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Time (UTCTime())
import qualified Data.Vector as V

-- | Gets the current "Funds" of the trader.
getFunds :: Jafar Funds
getFunds = gets jsFunds

-- | Gets the current position of the trader in the market.
getPosition :: Jafar Position
getPosition = gets jsPosition

-- | Mutates the "jsPosition" field of the "JafarState"
savePosition :: Position -> Jafar ()
savePosition p = modify $ \s -> s { jsPosition = p }

-- | Gets the current price of Bitcoin.
getPrice :: Jafar Price
getPrice = do
    p <- V.head <$> gets jsLastPrices
    return p

-- | Gets the price of Bitcoin effective on the last iteration of the
-- algorithm.
getPrevPrice :: Jafar Price
getPrevPrice = V.head <$> gets jsLastPrices -- TODO account for t_0

-- | Gets the stoploss value computed on the last iteration of the algorithm.
prevStopLoss :: Jafar Double
prevStopLoss = gets jsPrevStoploss

-- | Gets the sensitivity of the algorithm.
getSensitivity :: Jafar Sensitivity
getSensitivity = asks jcSensitivity

-- | Mutates the "jsPrevStoploss" field of the "JafarState".
saveStopLoss :: Double -> Jafar ()
saveStopLoss stoploss = modify $ \s -> s { jsPrevStoploss = stoploss }

-- | Gets the current "Trend" in the market by analyzing the "emaScore" of the
-- exponential moving averages held in the "jsEMA" backlog.
getTrend :: Jafar Trend
getTrend = do
    score <- V.sum . V.map emaScore <$> gets jsEMA
    return $
        if score >= 0
            then UpTrend
            else DownTrend

-- | Gets the "Trend" representing whether there is an EMA curve crossover.
getCrossover :: Jafar Trend
getCrossover = do
    score <- (fmap . fmap) emaScore . fmap (V.!? 0) $ gets jsEMA
    case score of
        Nothing -> error "no head"
        Just x -> return $
            if x >= 0
                then UpTrend
                else DownTrend

-- | Mutates the "JafarState"'s backlog of EMA curves held in "jsEMA".
setEMAs :: V.Vector EMA -> Jafar ()
setEMAs emas = modify $ \s -> s { jsEMA = emas }

-- | Adds a "Price" to the backlog of market prices, and updates the EMA
-- backlog.
addPrice :: Price -> Jafar (V.Vector EMA)
addPrice p = do
    (ssz, msz, lsz) <- asks jcEMASizes
    emas <- gets jsEMA
    ps <- gets jsLastPrices
    backlogSize <- asks jcEMABacklog

    let ps' = if V.length ps > max (max ssz msz) lsz
                  then p `V.cons` (V.init ps)
                  else p `V.cons` ps
        psS = V.take ssz ps'
        psM = V.take msz ps'
        psL = V.take lsz ps'
        avg v = V.sum v / fromIntegral (V.length v)
        s = avg psS
        m = avg psM
        l = avg psL
        emas' = if V.length emas > backlogSize
                    then EMA s m l `V.cons` (V.init emas)
                    else EMA s m l `V.cons` emas

    modify $ \s -> s { jsLastPrices = ps' }
    return emas'

-- | Gets the stoploss value for this iteration of the algorithm.
getStopLoss :: Jafar Double
getStopLoss = do
    pos <- getPosition
    prev <- prevStopLoss
    price <- getPrice
    prevPrice <- getPrevPrice
    sens <- getSensitivity
    if price > prev && pos == Long && sens >= 0 && sens <= 1
        then return ((1 - sens*0.2)*price)
    else if price < prev && pos == Short && sens >= 0 && sens <= 1
        then return ((1 + sens*0.2)*price)
    else return prev

-- | Interpret a "Program" in the "Jafar" monad.
interpret :: Action -> Jafar (Either Outcome Value)

interpret t = case t of
    GetTrend -> do
        trend <- getTrend
        return $ Right $ Trend trend

    GetCrossover -> do
        cross <- getCrossover
        return $ Right $ Trend cross

    GetPosition -> do
        pos <- getPosition
        return $ Right $ Position pos

    GetStopLoss -> do
        stoploss <- getStopLoss
        return $ Right $ Price stoploss

    GetPrice -> do
        price <- getPrice
        return $ Right $ Price price

    Emit o -> pure $ Left o

    If cond aThen aElse -> do
        Right condition <- interpret cond
        case condition of
            ValBool True -> interpret aThen
            ValBool False -> interpret aElse
            _ -> error "Error: Type error."

    Compare ord act1 act2 -> do
        let cmp a b = case ord of
                LT -> a < b
                EQ -> a == b
                GT -> a > b
        let eq a b = case ord of
                EQ -> a == b
                _ -> error "Error: Type error."

        Right a <- interpret act1
        Right b <- interpret act2

        return $ Right $ ValBool $
            case (a, b) of
                (ValBool x, ValBool y) -> cmp x y
                (Price x, Price y) -> cmp x y
                (Time x, Time y) -> cmp x y
                (ValString x, ValString y) -> cmp x y
                (Position x, Position y) -> eq x y
                (Trend x, Trend y) -> eq x y
                _ -> error "Error: type error."

    Lit val -> pure $ Right val

    Not act -> do
        Right a <- interpret act
        case a of
            ValBool b -> return $ Right (ValBool (not b))
            _ -> error "Error: Type error."

-- | Throws a "NoOutcome" error if the Either is constructed with "Right".
requireOutcomeLeft :: Either Outcome b -> Jafar Outcome
requireOutcomeLeft (Left o) = return o
requireOutcomeLeft (Right _) = throwError NoOutcome

-- | The main loop of the backtester. The given data is simply pumped until
-- none is left to determine the final funds held by the trader.
--
-- Each iteration of the loop will update the relevant fields of the
-- "JafarState", and record any transactions to the log.
jafarLoop :: Program -> [(UTCTime, Price)] -> Jafar Funds
jafarLoop _ [] = do
    finalfunds <- gets jsFunds
    return finalfunds

jafarLoop code (d:ds) = do
    -- run the trade decision procedure
    outcome <- interpret code >>= requireOutcomeLeft

    -- fetch the current price of Bitcoin; this value is recorded into the
    -- transactions stored in the log, so we need to fetch it now before
    -- updating the price list.
    currentPrice <- getPrice

    -- add the next price of Bitcoin to the list of prices, and compute the new
    -- EMA curves.
    emas <- addPrice (snd d)

    -- record the new EMA curves into the state.
    setEMAs emas

    -- fetch the current funds of the trader
    Funds currency holdings oldPrice <- getFunds

    -- branch on the signal generated by the trade decision procedure and on
    -- the currency currently held by the trader to determine whether the
    -- decision made by the trade decision procedure makes sense. If so,
    -- record the resulting transaction to the log and update the trader's
    -- funds.
    let m = case outcome of
            Buy Long u -> Just $
                case currency of
                    BTC -> error "wrong funds error in Buy Long"
                    USD -> let f = Funds BTC (holdings / currentPrice) currentPrice
                               s = Long
                               in (Transaction TxBuy BTC holdings currentPrice, f, u, s)

            Buy Short u -> Just $
                case currency of
                    USD -> error "Wrong funds error in buy short"
                    BTC -> let f = Funds USD
                                      (holdings * (2 * oldPrice - currentPrice))
                                      currentPrice
                               s = NoPosition
                               in (Transaction TxSell BTC holdings currentPrice, f, u, s)

            Sell Long u -> Just $
                case currency of
                    USD -> error "wrong funds error in sell long"
                    BTC -> let f = Funds USD (holdings * currentPrice) currentPrice
                               s = NoPosition
                               in (Transaction TxSell BTC holdings currentPrice, f, u, s)

            Sell Short u -> Just $
                case currency of
                    BTC -> error "wrong funds error in sell short"
                    USD -> let f = Funds BTC (holdings / currentPrice) currentPrice
                               s = Short
                               in (Transaction TxBuy BTC holdings currentPrice, f, u, s)

            DoNothing -> Nothing

    case m of
        Nothing -> return ()
        Just (t, f, u, s) -> do
            savePosition s
            modify $ \s -> s
                { jsFunds = f, jsTransactions = t : jsTransactions s }

    jafarLoop code ds

-- | Backtests an "Algorithm" with a given "InitialCondition" on a given
-- "BacktestDataset".
backtest :: Algorithm -> InitialCondition -> BacktestDataset -> IO BacktestResult
backtest alg ic (BacktestDataset { bdData = ds }) = do
    let js = initialJafarState ic (snd . head $ ds)
    let jc = JafarConf { jcSensitivity = algSensitivity alg
                       , jcTimeInterval = icTimeInterval ic
                       , jcStartingCapital = icStartingCapital ic
                       , jcEMASizes = (5, 9, 12)
                       , jcEMABacklog = 5
                       }

    putStrLn "starting Jafar"
    (e, js') <- runStateT
             (runReaderT
                 (runExceptT (runJafar $ do
                                         (jafarLoop (algCode alg) ds)))
             jc)
         js

    case e of
        Left error -> do
            putStrLn $ "Error occurred: " ++ show error
            return ResultFailure
        Right s -> do
            print s
            return ResultSuccess
