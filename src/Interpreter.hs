module Interpreter (runProgram) where

import Parser
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Except

data Types = IntType Int | ArrayType [Int] | FType [String] [Stmt] Expr | PrintLog [String] deriving Show
type SymbolTable = Map.Map String Types
type Index = Int

data RuntimeError = UndeclaredVariable String
    | UndeclaredFunction String
    | VariableNotAnArray String
    | NonPositivArraySize Int String
    | NegativeIndex Int String
    | IndexOutOfBounds Int String
    | DivideByZero

instance Show RuntimeError where
    show (UndeclaredVariable s)     = "[Error] There was an attempt to use an undeclared variable " ++ s
    show (UndeclaredFunction s)     = "[Error] There was an attempt to call an undeclared function " ++ s
    show (VariableNotAnArray s)     = "[Error] There was an attempt to use variable " ++ s ++ "as an array, when it was not defined as such"
    show (NonPositivArraySize i s)  = "[Error] There was an attempt to create an array " ++ s ++ " with size " ++ (show i) ++ ". All array sizes must be larger than 0"
    show (NegativeIndex i s)        = "[Error] Negative indexes are currently not supported: " ++ s ++ "[" ++ (show i) ++ "]."
    show (IndexOutOfBounds i s)     = "[Error] Index " ++ (show i) ++ " is out of bounds for array " ++ s
    show (DivideByZero)             = "[Error] Divide by zero is undefined"

type ExceptionMonad = ExceptT String IO

runProgram :: [Stmt] -> ExceptionMonad()
runProgram stmts = foldM interprit Map.empty stmts >>= (\st -> liftIO $ print st) 

updateNth :: Int -> a -> [a] -> [a]
updateNth _ _ [] = error "Error in interpreter: updateNth reached []. please contact developer"
updateNth n v (x:xs)
    | n == 0    = v : xs
    | otherwise = x : updateNth (n-1) v xs

updateList :: Index -> Int -> String -> [Int] -> ExceptionMonad [Int]
updateList _ _ _ []    = error "Error in interpreter: updateList reached []. please contact developer"
updateList i v n xs
    | length xs <= i = throwError $ show $ IndexOutOfBounds i n
    | i < 0          = throwError $ show $ NegativeIndex i n
    | otherwise      = return $ updateNth i v xs

interprit :: SymbolTable -> Stmt -> ExceptionMonad(SymbolTable)
interprit st (Assign n expr) = evalExpr expr st >>= (\val -> return $ Map.insert n (IntType val) st)
interprit st (AssignIndex (Index name index) expr) =
    do
        i <- (evalExpr index st)
        v <- (evalExpr expr st)
        case Map.lookup name st of
            Just (ArrayType xs) -> updateList i v name xs >>= (\newArr -> return $ Map.insert name (ArrayType newArr) st)
            _                   -> throwError $ show $ VariableNotAnArray name

interprit st (Function name args stmts ret) =
    return $ Map.insert name (FType args stmts ret) st

interprit st (If cond stmts) = do
    c <- evalCond cond st
    if c
        then foldM interprit st stmts
        else return $ st

interprit st (While cond stmts) = do
    c <- evalCond cond st
    case c of
        True -> foldM interprit st stmts >>= (\nst -> interprit nst (While cond stmts))
        False -> return $ st

interprit st (Print expr) = evalExpr expr st >>= liftIO.print >> return st

interprit st (PrintStr str) = liftIO (print str) >> return st

interprit st (InitArray n expr) = do
    len <- evalExpr expr st
    if len <= 0
        then throwError $ show $ NonPositivArraySize len n
        else return $ Map.insert n (ArrayType (replicate len 0)) st

evalBiOp :: (Int -> Int -> Int) -> Expr -> Expr -> SymbolTable -> ExceptionMonad(Int)
evalBiOp f expr1 expr2 st = do
    e1 <- evalExpr expr1 st
    e2 <- evalExpr expr2 st
    return $ (f) e1 e2

evalExpr :: Expr -> SymbolTable -> ExceptionMonad(Int)
evalExpr (Var name) st =
    case Map.lookup name st of
        Just (IntType val) -> return val
        _ -> throwError $ show $ UndeclaredVariable name
                
evalExpr (Index name expr) st = do
    val <- evalExpr expr st
    case Map.lookup name st of
        Just (ArrayType xs) -> if length xs > val
                                    then return $ xs !! val
                                    else throwError $ show $ IndexOutOfBounds val name
        _ -> throwError $ show $ VariableNotAnArray name

evalExpr (Call name args) st =
    case Map.lookup name st of
        Just (FType arg_names stmts ret) -> runFunction >>= evalExpr ret 
            where
                runFunction = do
                    eval_args <- mapM (\e -> evalExpr e st) args
                    let named_args = zip arg_names eval_args
                    fstmt <- foldM interprit st $ [Assign n (Int v) | (n,v) <- named_args]
                    foldM interprit fstmt stmts
        _ -> throwError $ show $ UndeclaredFunction name
        


evalExpr (Int v) _ = return v
evalExpr (Negation expr) st = evalExpr expr st >>= (return.negate)
evalExpr (Sum expr1 expr2) st = evalBiOp (+) expr1 expr2 st  
evalExpr (Subtr expr1 expr2) st = evalBiOp (-) expr1 expr2 st  
evalExpr (Product expr1 expr2) st = evalBiOp (*) expr1 expr2 st  
evalExpr (Division expr1 expr2) st = do
    e1 <- evalExpr expr1 st
    e2 <- evalExpr expr2 st
    if e2 == 0
        then throwError (show DivideByZero)
        else return $ e1 `div` e2

evalCond' :: (Int -> Int -> Bool) -> Expr -> Expr -> SymbolTable -> ExceptionMonad(Bool)
evalCond' f e1 e2 st = do
    v1 <- evalExpr e1 st
    v2 <- evalExpr e2 st
    return $ (f) v1 v2

evalCond :: Cond -> SymbolTable -> ExceptionMonad(Bool)
evalCond (Great e1 e2) st = evalCond' (>) e1 e2 st
evalCond (Less e1 e2) st =  evalCond' (<) e1 e2 st
evalCond (Equal e1 e2) st = evalCond' (==) e1 e2 st