module ExampleLanguage where

data Value
    = ValString String
    | ValInt Int
    | ValBool Bool
    | ValNothing
    deriving (Show)
data Action
    = Show Action
    | Add Action Action
    | If Action Action Action
    | LessThan Action Action
    | Literal Value
    deriving (Show)

data Typesafety
    = TypeSafe Type
    | NotTypeSafe
    deriving (Show)

data Type
    = TypeString
    | TypeInt
    | TypeBool
    | TypeNothing
    deriving (Eq, Show)

typecheck :: Action -> Typesafety
typecheck (Literal (ValString _)) = TypeSafe TypeString
typecheck (Literal (ValInt _)) = TypeSafe TypeInt
typecheck (Literal (ValBool _)) = TypeSafe TypeBool
typecheck (Literal (ValNothing)) = TypeSafe TypeNothing

typecheck (Add e1 e2) = case (typecheck e1, typecheck e2) of
    (TypeSafe TypeInt, TypeSafe TypeInt) -> TypeSafe TypeInt
    _ -> NotTypeSafe

typecheck (Show e) = case typecheck e of
    TypeSafe TypeString -> TypeSafe TypeNothing
    _ -> NotTypeSafe

typecheck (LessThan e1 e2) = case (typecheck e1, typecheck e2) of
    (TypeSafe TypeInt, TypeSafe TypeInt) -> TypeSafe TypeBool
    _ -> NotTypeSafe

typecheck (If cond aThen aElse) = case typecheck cond of
    TypeSafe TypeBool -> case (typecheck aThen, typecheck aElse) of
        (TypeSafe x, TypeSafe y) -> if x == y then TypeSafe x else NotTypeSafe
        _ -> NotTypeSafe

interpret :: Action -> IO Value

interpret (Show a) = do
    x <- interpret a
    case x of
        ValString s -> do
            putStrLn s
        _ -> error "Type error"
    return ValNothing

interpret (Add e1 e2) = do
    v1 <- interpret e1
    v2 <- interpret e2
    case (v1, v2) of
        (ValInt i1, ValInt i2) ->
            return $ ValInt (i1 + i2)
        _ -> error "Type error"

interpret (LessThan e1 e2) = do
    v1 <- interpret e1
    v2 <- interpret e2
    case (v1, v2) of
        (ValInt i1, ValInt i2) ->
            if i1 < i2
                then return (ValBool True)
                else return (ValBool False)
        _ -> error "Type error"

interpret (If aBool aThen aElse) = do
    b <- interpret aBool
    case b of
        ValBool b ->
            if b
                then interpret aThen
                else interpret aElse
        _ -> error "Type error"

interpret (Literal v) = return v


simplify :: Action -> Action
simplify (Add x y) = case (simplify x, simplify y) of
                         (Literal (ValInt x), Literal (ValInt y)) ->
                             Literal (ValInt (x + y))
simplify x = x

addition = (Add (Literal $ ValInt 3)
                (Add (Literal $ ValInt 4)
                     (Add (Add (Literal (ValInt 6))
                               (Literal (ValInt 5)))
                          (Add (Literal (ValInt 7))
                               (Literal (ValInt 8))))))

simpleAddition = simplify addition

tree = If (LessThan (Add (Literal $ ValInt 2)
                    (Add (Literal $ ValInt 3)
                         (Literal $ ValInt 2)))
               (Literal $ ValInt 8))
     (Show (Literal (ValString "Is less")))
     (Show (Literal (ValString "Is not less")))

simplifiedTree = simplify tree

main =
    case typecheck tree of
        TypeSafe _ -> do
            interpret tree
            return ()
        _ -> putStrLn "Not type safe!"
