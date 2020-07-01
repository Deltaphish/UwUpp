module Interpreter (runProgram) where

import Parser
import qualified Data.Map as Map
import Control.Monad

data Types = IntType Int | ArrayType [Int] | FType [String] [Stmt] Expr | PrintLog [String]
type SymbolTable = Map.Map String Types
type Index = Int

runProgram :: [Stmt] -> IO()
runProgram stmts = foldM_ interprit Map.empty stmts

updateNth :: Int -> a -> [a] -> [a]
updateNth _ _ [] = []
updateNth n v (x:xs)
    | n == 0    = v : xs
    | otherwise = x : updateNth (n-1) v xs

updateList :: Index -> Int -> [Int] -> [Int]
updateList _ _ []    = []
updateList i v xs
    | length xs <= i = updateList i v $ replicate (i+1) 0
    | i < 0          = updateList (length xs - (i + 1)) v xs
    | otherwise      = updateNth i v xs

interprit :: SymbolTable -> Stmt -> IO(SymbolTable)
interprit st (Assign n expr) = evalExpr expr st >>= (\val -> return $ Map.insert n (IntType val) st)
interprit st (AssignIndex (Index name index) expr) =
    do
        i <- (evalExpr index st)
        v <- (evalExpr expr st)
        return $ Map.insert name (ArrayType (updateList i v xs)) st
            where xs = case Map.lookup name st of
                        Just (ArrayType xs) -> xs
                        _ -> []

interprit st (Function name args stmts ret) =
    return $ Map.insert name (FType args stmts ret) st

interprit st (If cond stmts) = do
    c <- evalCond cond st
    case c of
        True -> foldM interprit st stmts
        False -> return $ st

interprit st (While cond stmts) = do
    c <- evalCond cond st
    case c of
        True -> foldM interprit st stmts >>= (\nst -> interprit nst (While cond stmts))
        False -> return $ st

interprit st (Print expr) = evalExpr expr st >>= print >> return st

interprit st (PrintStr str) = print str >> return st

interprit st (InitArray n expr) = do
    len <- evalExpr expr st
    return $ validateInitArray len n st

validateInitArray :: Int -> String -> SymbolTable -> SymbolTable
validateInitArray len name st
    | len <= 0 = st
    | otherwise = Map.insert name (ArrayType (replicate len 0)) st

evalBiOp :: (Int -> Int -> Int) -> Expr -> Expr -> SymbolTable -> IO(Int)
evalBiOp f expr1 expr2 st = do
    e1 <- evalExpr expr1 st
    e2 <- evalExpr expr2 st
    return $ (f) e1 e2

evalExpr :: Expr -> SymbolTable -> IO(Int)
evalExpr (Var name) st =
    return $ case Map.lookup name st of
                Just (IntType val) -> val
                _ -> 0
evalExpr (Index name expr) st = do
    val <- evalExpr expr st
    return $ case Map.lookup name st of
                Just (ArrayType xs) -> case length xs <= val of
                                            False ->  xs !! val
                                            True -> -1
                _ -> 0

evalExpr (Call name args) st =
    case Map.lookup name st of
        Just (FType arg_names stmts ret) -> runFunction >>= evalExpr ret 
            where
                runFunction = do
                    eval_args <- mapM (\e -> evalExpr e st) args
                    let named_args = zip arg_names eval_args
                    fstmt <- foldM interprit st $ [Assign n (Int v) | (n,v) <- named_args]
                    foldM interprit fstmt stmts


evalExpr (Int v) _ = return v
evalExpr (Negation expr) st = evalExpr expr st >>= (return.negate)
evalExpr (Sum expr1 expr2) st = evalBiOp (+) expr1 expr2 st  
evalExpr (Subtr expr1 expr2) st = evalBiOp (-) expr1 expr2 st  
evalExpr (Product expr1 expr2) st = evalBiOp (*) expr1 expr2 st  
evalExpr (Division expr1 expr2) st = evalBiOp (div) expr1 expr2 st  

evalCond' :: (Int -> Int -> Bool) -> Expr -> Expr -> SymbolTable -> IO(Bool)
evalCond' f e1 e2 st = do
    v1 <- evalExpr e1 st
    v2 <- evalExpr e2 st
    return $ (f) v1 v2

evalCond :: Cond -> SymbolTable -> IO(Bool)
evalCond (Great e1 e2) st = evalCond' (>) e1 e2 st
evalCond (Less e1 e2) st =  evalCond' (<) e1 e2 st
evalCond (Equal e1 e2) st = evalCond' (==) e1 e2 st