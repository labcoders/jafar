module Interpreter where

import Types
import Sugar

import Control.Applicative hiding (empty)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Vector as V

getTrend :: Jafar Trend
getTrend = do
    score <- V.sum . V.map emaScore <$> gets jsEMA
    return $
        if score > 0
            then UpTrend
            else DownTrend

getCrossover :: Jafar Trend
getCrossover = do
    score <- (fmap . fmap) emaScore . fmap (V.!? 0) $ gets jsEMA
    case score of
        Nothing -> error "no head"
        Just x -> return $
            if x > 0
                then UpTrend
                else DownTrend

addPrice :: Price -> Jafar ()
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

    modify $ \s -> s { jsEMA = emas' }

getPosition :: Jafar Position
getPosition = gets jsPosition

savePosition :: Position -> Jafar ()
savePosition p = modify $ \s -> s { jsPosition = p }

getPrice :: Jafar Price
getPrice = undefined

getPrevPrice :: Jafar Price
getPrevPrice = V.head <$> gets jsLastPrices -- TODO account for t_0

prevStopLoss :: Jafar Double
prevStopLoss = gets jsPrevStoploss

getSensitivity :: Jafar Sensitivity
getSensitivity = asks jcSensitivity

saveStopLoss :: Double -> Jafar ()
saveStopLoss stoploss = modify $ \s -> s { jsPrevStoploss = stoploss }

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

interpret :: Action -> Jafar (Either Outcome Value)
interpret GetTrend = do
    trend <- getTrend
    return $ Right $ Trend trend

interpret GetCrossover = do
    cross <- getCrossover
    return $ Right $ Trend cross

interpret GetPosition = do
    pos <- getPosition
    return $ Right $ Position pos

interpret GetStopLoss = do
    stoploss <- getStopLoss
    return $ Right $ Price stoploss

interpret GetPrice = do
    price <- getPrice
    return $ Right $ Price price

interpret (Emit o) = return $ Left o

interpret (If cond aThen aElse) = do
    Right condition <- interpret cond
    case condition of
        ValBool True -> interpret aThen
        ValBool False -> interpret aElse
        _ -> error "Error: Type error."

interpret (Compare ord act1 act2) = do
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

interpret (Lit val) = return $ Right val

interpret (Not act) = do
    Right a <- interpret act
    case a of
        ValBool b -> return $ Right (ValBool (not b))
        _ -> error "Error: Type error."

requireOutcomeLeft :: Either Outcome b -> Jafar Outcome
requireOutcomeLeft (Left o) = return o
requireOutcomeLeft (Right _) = throwError NoOutcome

backtest :: Algorithm -> InitialCondition -> BacktestDataset -> IO BacktestResult
backtest alg ic ds = do
    let js = initialJafarState ic (snd . head . bdData $ ds)
    let jc = JafarConf { jcSensitivity = algSensitivity alg
                       , jcTimeInterval = icTimeInterval ic
                       , jcStartingCapital = icStartingCapital ic
                       , jcEMASizes = (5, 9, 12)
                       , jcEMABacklog = 5
                       }
    (e, js') <- runStateT
             (runReaderT
                 (runExceptT
                     (runJafar $
                         interpret (algCode alg) >>= requireOutcomeLeft))
             jc)
         js
    case e of
        Left error -> do
            putStrLn $ "Error occurred: " ++ show error
            undefined
        Right s -> do
            print s

    return ResultSuccess
