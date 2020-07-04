module Interpreter (runProgram) where

import Parser
import AST
import Runtime
import Type
import StdLib
import qualified Data.Map as Map
import Data.Ord
import Data.Maybe
import Control.Monad
import Control.Monad.Except

runProgram :: [Stmt] -> Runtime()
runProgram stmts = foldM_ interpret initST stmts

nativeWrapper :: [Name] -> [Stmt] -> Expr -> [Type] -> SymbolTable -> Runtime(Type)
nativeWrapper arg_names body ret args st = runFunction >>= evalExpr ret 
    where   
        runFunction = do
            let argPairs = zip arg_names args
            let fstmt = foldl (\m (n,v) -> Map.insert n v m) st argPairs 
            foldM interpret fstmt body

interpret :: SymbolTable -> Stmt -> Runtime(SymbolTable)
interpret st (Assign n expr) = evalExpr expr st >>= (\val -> return $ Map.insert n val st)
interpret st (AssignIndex (Index name index) expr) =
    do
        i <- (evalExpr index st)
        v <- (evalExpr expr st)
        case Map.lookup name st of
            Just arr -> put arr i v >>= (\v' -> return $ Map.insert name v' st)
            _ -> throwError $ UndeclaredVariable name

interpret st (Function name args stmts ret) =
    return $ Map.insert name (FType (nativeWrapper args stmts ret)) st

interpret st (If cond stmts) = do
    c <- evalCond cond st
    if c
        then foldM interpret st stmts
        else return $ st

interpret st (While cond stmts) = do
    c <- evalCond cond st
    if c
        then foldM interpret st stmts >>= (\nst -> interpret nst (While cond stmts))
        else return $ st

interpret st (Print expr) = evalExpr expr st >>= liftIO.print.fromJust.typeShow >> return st

interpret st (PrintStr str) = liftIO (print str) >> return st

interpret st (InitArray n expr ArrayTypeString) = do
    v <- evalExpr expr st
    case v of
        (IntType len) -> if len <= 0
                        then throwError $ NonPositivArraySize len n
                        else return $ Map.insert n (StrArrayType (replicate len (StrType ""))) st
        _ -> throwError NonIntIndex

interpret st (InitArray n expr ArrayTypeInt) = do
    v <- evalExpr expr st
    case v of
        (IntType len) -> if len <= 0
                        then throwError $ NonPositivArraySize len n
                        else return $ Map.insert n (IntArrayType (replicate len (IntType 0))) st
        _ -> throwError NonIntIndex

interpret st (FunctionCall n exprs) = do
    case Map.lookup n st of
        Just (FType f) -> do
            eval_args <- mapM (\e -> evalExpr e st) exprs
            f eval_args st
            return st
        _ -> throwError $ UndeclaredFunction n

evalBi :: Expr -> Expr -> SymbolTable -> Runtime((Type,Type))
evalBi expr1 expr2 st = do
    e1 <- evalExpr expr1 st
    e2 <- evalExpr expr2 st
    return (e1,e2)

evalExpr :: Expr -> SymbolTable -> Runtime(Type)
evalExpr (Var name) st =
    case Map.lookup name st of
        Just v -> return v
        _ -> throwError $ UndeclaredVariable name
                
evalExpr (Index name expr) st = do
    val <- evalExpr expr st
    case Map.lookup name st of
        Just arr -> get arr val
        Nothing -> throwError $ UndeclaredVariable name

evalExpr (Call name args) st =
    case Map.lookup name st of
        Just (FType f) -> do
            eval_args <- mapM (\e -> evalExpr e st) args
            f eval_args st
        _ -> throwError $ UndeclaredFunction name
        


evalExpr (Int v) _ = return $ IntType v
evalExpr (Str v) _ = return $ StrType v
evalExpr (Negation expr) st = do 
    v <- evalExpr expr st
    case v of
        IntType i -> return $ IntType $ negate i
        _ -> throwError $ NaNNegate

evalExpr (Sum expr1 expr2) st = do 
    (e1,e2) <- evalBi expr1 expr2 st
    case (e1,e2) of
        (IntType i1, IntType i2) -> return $ IntType ( i1 + i2 )
        (IntType i1, StrType s) -> return $ StrType $ (show i1) ++ s
        (StrType s, IntType i2) -> return $ StrType $ s ++ (show i2)
        (StrType s1, StrType s2) -> return $ StrType $ s1 ++ s2
        _ -> throwError AddNotSupported

evalExpr (Subtr expr1 expr2) st = do
    (e1,e2) <- evalBi expr1 expr2 st
    case (e1,e2) of
        (IntType i1, IntType i2) -> return $ IntType ( i1 - i2 )
        _ -> throwError SubNotSupported

evalExpr (Product expr1 expr2) st = do
    (e1,e2) <- evalBi expr1 expr2 st
    case (e1,e2) of
        (IntType i1, IntType i2) -> return $ IntType ( i1 * i2 )
        _ -> throwError MulNotSupported

evalExpr (Division expr1 expr2) st = do
    (e1,e2) <- evalBi expr1 expr2 st
    case (e1,e2) of
        (IntType i1, IntType i2) -> if i2 == 0
                                then throwError DivideByZero
                                else return $ IntType $ i1 `div` i2
        _ -> throwError DivNotSupported


evalCond' :: Ordering -> Expr -> Expr -> SymbolTable -> Runtime(Bool)
evalCond' o expr1 expr2 st = do
    (e1,e2) <- evalBi expr1 expr2 st
    ord <- cmpType e1 e2
    return $ ord == o

evalCond :: Cond -> SymbolTable -> Runtime(Bool)
evalCond (Great e1 e2) st = evalCond' GT e1 e2 st
evalCond (Less e1 e2) st =  evalCond' LT e1 e2 st
evalCond (Equal e1 e2) st = evalCond' EQ e1 e2 st