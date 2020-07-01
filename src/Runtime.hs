module Runtime where

import Control.Monad.Except
import AST

data RuntimeError = UndeclaredVariable Name
    | UndeclaredFunction Name
    | VariableNotAnArray Name
    | NonPositivArraySize Index Name
    | NegativeIndex Index Name
    | IndexOutOfBounds Index Name
    | DivideByZero

instance Show RuntimeError where
    show (UndeclaredVariable s)     = "[Error] There was an attempt to use an undeclared variable " ++ s
    show (UndeclaredFunction s)     = "[Error] There was an attempt to call an undeclared function " ++ s
    show (VariableNotAnArray s)     = "[Error] There was an attempt to use variable " ++ s ++ "as an array, when it was not defined as such"
    show (NonPositivArraySize i s)  = "[Error] There was an attempt to create an array " ++ s ++ " with size " ++ (show i) ++ ". All array sizes must be larger than 0"
    show (NegativeIndex i s)        = "[Error] Negative indexes are currently not supported: " ++ s ++ "[" ++ (show i) ++ "]."
    show (IndexOutOfBounds i s)     = "[Error] Index " ++ (show i) ++ " is out of bounds for array " ++ s
    show (DivideByZero)             = "[Error] Divide by zero is undefined"

uwuShow :: RuntimeError -> String
uwuShow (UndeclaredVariable s)     = "[Ewwow] Thewe was an attwempt tuwu use an undecwawed vawiabwe " ++ s
uwuShow (UndeclaredFunction s)     = "[Ewwow] Thewe was an attwempt tuwu caww an undecwawed functiown " ++ s
uwuShow (VariableNotAnArray s)     = "[Ewwow] Thewe was an attwempt tuwu use vawiabwe " ++ s ++ "as an awway, whewn iwt was nowt defined as such"
uwuShow (NonPositivArraySize i s)  = "[Ewwow] Thewe was an attempt tuwu cweate an awway " ++ s ++ " wif sise " ++ (show i) ++ ". Aww awway sizes must bwe wawgew than 0"
uwuShow (NegativeIndex i s)        = "[Ewwow] Negatwive indexwes awe cuwwentwy nowt suppowted UwU: " ++ s ++ "[" ++ (show i) ++ "]."
uwuShow (IndexOutOfBounds i s)     = "[Ewwow] Indwex " ++ (show i) ++ " iws out of bounds fow awway " ++ s
uwuShow (DivideByZero)             = "[Ewwow] Divide by zewo iws undefined"


type Runtime = ExceptT RuntimeError IO

reportResult :: Bool -> Either RuntimeError () -> IO()
reportResult _     (Right _ ) = return ()
reportResult False   (Left e )  = print ("UwU? is dis a wuntwime ewwow?: " ++ uwuShow e)
reportResult True (Left e )  = print ("The program failed to execute: "++ show e)  