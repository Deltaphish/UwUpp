module Runtime where

import Control.Monad.Except
import AST

data RuntimeError = UndeclaredVariable Name
    | UndeclaredFunction Name
    | VariableNotAnArray
    | NonPositivArraySize Index Name
    | NegativeIndex Index Name
    | IndexOutOfBounds Index
    | NonIntIndex
    | PrintFunction
    | NoArguments
    | TooManyArgs
    | CastToIntFail String
    | NaNNegate
    | AddNotSupported
    | SubNotSupported
    | MulNotSupported
    | DivNotSupported
    | CmpNotSupported
    | DivideByZero
    | NaNError
    deriving Show
{-
instance Show RuntimeError where
    show (UndeclaredVariable s)     = "[Error] There was an attempt to use an undeclared variable " ++ s
    show (UndeclaredFunction s)     = "[Error] There was an attempt to call an undeclared function " ++ s
    show (VariableNotAnArray)     = "[Error] There was an attempt to index an non-array variable"
    show (NonPositivArraySize i s)  = "[Error] There was an attempt to create an array " ++ s ++ " with size " ++ (show i) ++ ". All array sizes must be larger than 0"
    show (NonIntIndex)             = "[Error] There was an attempt to index an array with non-integer values"
    show (NegativeIndex i s)        = "[Error] Negative indexes are currently not supported: " ++ s ++ "[" ++ (show i) ++ "]."
    show (IndexOutOfBounds i )     = "[Error] Index " ++ (show i) ++ " is out of bounds"
    show (DivideByZero)             = "[Error] Divide by zero is undefined"

uwuShow :: RuntimeError -> String
uwuShow (UndeclaredVariable s)     = "[Ewwow] Thewe was an attwempt tuwu use an undecwawed vawiabwe " ++ s
uwuShow (UndeclaredFunction s)     = "[Ewwow] Thewe was an attwempt tuwu caww an undecwawed functiown " ++ s
uwuShow (VariableNotAnArray)     = "[Ewwow] Thewe was an attwempt tuwu use a vawiabwe as an awway, whewn iwt was nowt defined as such"
uwuShow (NonPositivArraySize i s)  = "[Ewwow] Thewe was an attempt tuwu cweate an awway " ++ s ++ " wif sise " ++ (show i) ++ ". Aww awway sizes must bwe wawgew than 0"
uwuShow (NegativeIndex i s)        = "[Ewwow] Negatwive indexwes awe cuwwentwy nowt suppowted UwU: " ++ s ++ "[" ++ (show i) ++ "]."
uwuShow (IndexOutOfBounds i)     = "[Ewwow] Indwex " ++ (show i) ++ " iws out of bounds fow awway"
uwuShow (DivideByZero)             = "[Ewwow] Divide by zewo iws undefined"
-}

type Runtime = ExceptT RuntimeError IO

{-
reportResult :: Bool -> Either RuntimeError () -> IO()
reportResult _     (Right _ ) = return ()
reportResult False   (Left e )  = print ("UwU? is dis a wuntwime ewwow?: " ++ uwuShow e)
reportResult True (Left e )  = print ("The program failed to execute: "++ show e)
-}