module Interpreter (runProgram) where

import Parser
import qualified Data.Map as Map

data Types = IntType Int | ArrayType [Int] | FType [String] [Stmt] Expr | PrintLog [String]
type SymbolTable = Map.Map String Types
type Index = Int

runProgram :: [Stmt] -> [String]
runProgram stmts = 
    case printlog of
            Just (PrintLog xs) -> xs
            Nothing -> [] 
    where
        endState = foldl interprit Map.empty stmts
        printlog = Map.lookup "printlog" endState

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

interprit :: SymbolTable -> Stmt ->  SymbolTable
interprit st (Assign n expr) = Map.insert n (IntType (evalExpr expr st)) st
interprit st (AssignIndex (Index name index) expr) = Map.insert name (ArrayType (updateList (evalExpr index st) (evalExpr expr st) xs)) st
    where xs = case Map.lookup name st of
                    Just (ArrayType xs) -> xs
                    _ -> []
interprit st (Function name args stmts ret) =
    Map.insert name (FType args stmts ret)  st

interprit st (If cond stmts)
    | evalCond cond st = foldl interprit st stmts
    | otherwise        = st

interprit st (While cond stmts)
    | evalCond cond st = interprit (foldl interprit st stmts) (While cond stmts)
    | otherwise        = st

interprit st (Print expr) = Map.insert "printlog" val  st
    where
        val = PrintLog (show value : log)
        log = case Map.lookup "printlog" st of
            Just (PrintLog ps) -> ps
            Nothing -> []
        value = evalExpr expr st

interprit st (PrintStr str) = Map.insert "printlog" val  st
    where
        val = PrintLog (str : log)
        log = case Map.lookup "printlog" st of
            Just (PrintLog ps) -> ps
            Nothing -> []

interprit st (InitArray n expr)
    | len <= 0 = st
    | otherwise = Map.insert n (ArrayType (replicate len 0)) st
    where
        len = evalExpr expr st

evalBiOp :: (Int -> Int -> Int) -> Expr -> Expr -> SymbolTable -> Int
evalBiOp f expr1 expr2 st = (f) (evalExpr expr1 st) (evalExpr expr2 st) 

evalExpr :: Expr -> SymbolTable -> Int
evalExpr (Var name) st =
    case Map.lookup name st of
        Just (IntType val) -> val
        _ -> 0
evalExpr (Index name expr) st =
    case Map.lookup name st of
        Just (ArrayType xs) -> case length xs <= val of
                                    False ->  xs !! val
                                    True -> -1
                                where
                                    val = evalExpr expr st
        _ -> 0

evalExpr (Call name args) st =
    case Map.lookup name st of
        Just (FType arg_names stmts ret) -> evalExpr ret runFunction
            where
                runFunction = foldl interprit fstmt stmts
                fstmt = foldl interprit st $ [Assign n (Int v) | (n,v) <- named_args]
                named_args = zip arg_names eval_args
                eval_args = map (\e -> evalExpr e st) args

evalExpr (Int v) _ = v
evalExpr (Negation expr) st = negate $ evalExpr expr st
evalExpr (Sum expr1 expr2) st = evalBiOp (+) expr1 expr2 st  
evalExpr (Subtr expr1 expr2) st = evalBiOp (-) expr1 expr2 st  
evalExpr (Product expr1 expr2) st = evalBiOp (*) expr1 expr2 st  
evalExpr (Division expr1 expr2) st = evalBiOp (div) expr1 expr2 st  

evalCond :: Cond -> SymbolTable -> Bool
evalCond (Great e1 e2) st = (evalExpr e1 st) > (evalExpr e2 st)
evalCond (Less e1 e2) st = (evalExpr e1 st) < (evalExpr e2 st)
evalCond (Equal e1 e2) st = (evalExpr e1 st) == (evalExpr e2 st)