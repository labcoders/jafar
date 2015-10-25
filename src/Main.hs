module Main where
import Data.Time.Clock

getTrend = undefined
getCrossover = undefined
getPosition = undefined
savePosition = undefined
getPrice = undefined
prevStopLoss = undefined
getSensitivity = undefined
saveStopLoss = undefined

data Trend
    = UpTrend
    | DownTrend
        deriving (Eq)

data Urgency
    = NotUrgent
    | Urgent
        deriving (Eq)

data Outcome
    = Buy Position Urgency
    | Sell Position Urgency
    | DoNothing
        deriving (Eq)

data Position
    = Long
    | Short
    | NoPosition
        deriving (Eq)

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
        deriving (Eq)

data Value
    = Price Double
    | ValBool Bool
    | Time UTCTime
    | ValString String
    | Position Position
    | Trend Trend
        deriving (Eq)

type Sensitivity = Double
type Price = Double

-- saveStopLoss :: Double -> IO ()
-- saveStopLoss stoploss = do
    -- SOMETHING IN HASKELL

getStopLoss :: IO (Double)
getStopLoss = do
    pos <- getPosition
    prev <- prevStopLoss
    price <- getPrice
    sens <- getSensitivity
    if price > prev && pos == Long && sens >= 0 && sens <= 1
        then return ((1 - sens*0.2)*price)
    else if price < prev && pos == Short && sens >= 0 && sens <= 1
        then return ((1 + sens*0.2)*price)
    else return prev

interpret :: Action -> IO (Either Outcome Value)
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

a1 #># a2 = Compare GT a1 a2
a1 #==# a2 = Compare EQ a1 a2
a1 #!=# a2 = Not (Compare EQ a1 a2)
a1 #<# a2 = Compare LT a1 a2
buy pos urg = Emit (Buy pos urg)
sell pos urg = Emit (Sell pos urg)
pass = Emit DoNothing
pos a = Lit (Position a)
trnd a = Lit (Trend a)

emaProg = If (GetPosition #==# (pos Long))
             (If (GetPrice #># GetStopLoss)
                 (If (GetTrend #==# (trnd UpTrend))
                     pass
                     (sell Long NotUrgent))
                 (sell Long Urgent))
             (If (GetPosition #==# (pos Short))
                 (If (GetPrice #<# GetStopLoss)
                     (If (GetTrend #==# (trnd DownTrend))
                         pass
                         (buy Short NotUrgent))
                     (buy Short Urgent))
                 (If (GetTrend #!=# GetCrossover)
                     (If (GetCrossover #==# (trnd UpTrend))
                         (buy Long NotUrgent)
                         (sell Short NotUrgent))
                     pass))

main = do
        putStrLn "Hello world!"
