module UwU.Backend.Interpreter.StdLib (initST) where

-- STD library functions implemented in Haskell for use in UwU++ programs
-- The "S" in STD means nothing, expect that these functions and there signatures will change.


import Control.Monad.Except (liftIO,throwError)
import UwU.Backend.Interpreter.TypeHelpers
import UwU.Backend.Interpreter.Runtime
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.List.Split (splitOn)

initST :: SymbolTable
initST = Map.fromList init
    where init = [ ("dumpState", FType dump_st)
                , ("nuzzels",FType std_print)
                , ("wisten",FType std_read)
                , ("wistenInt",FType std_readInt)
                , ("wead", FType std_readFile)
                , ("wite", FType std_writeFile)
                , ("conwat", FType std_concat)
                , ("spwit", FType std_split)
                , ("wength", FType std_length)]

-- debug

dump_st :: Function
dump_st _ st = liftIO (print st) >> return (IntType 0)

-- Array

std_length :: Function
std_length [] _ = throwError NoArguments
std_length ((IntArrayType xs) : []) _ = return $ IntType $ length xs
std_length ((StrArrayType xs) : []) _ = return $ IntType $ length xs
std_length _ _ = throwError $ CustomError "Cant get length of a non array type" 


-- IO

std_print :: Function
std_print [] _ = throwError NoArguments
std_print a _ = do
    liftIO $ putStrLn $ unwords $ mapMaybe typeShow a
    return $ IntType 0

std_read :: Function
std_read [] _ = do
    s <- liftIO getLine
    return $ StrType s

std_readInt :: Function
std_readInt [] _ = do
    i <- liftIO readLn :: Runtime Int
    return $ IntType i

std_readFile :: Function
std_readFile [] _ = throwError $ NoArguments
std_readFile ((StrType f) : []) _ = do
    c <- liftIO (readFile f) :: Runtime String
    let l = lines c
    return $ StrArrayType $ map StrType l
std_readFile _ _ = throwError $ InvalidArguments

std_writeFile :: Function
std_writeFile [] _ = throwError $ NoArguments
std_writeFile ((StrType p) : (StrType s) : []) _ = do
    liftIO $ writeFile p s
    return $ IntType 0

std_writeFile (p : (StrArrayType ss) : []) st = 
    std_writeFile [p,StrType (unlines $ map extract ss)] st
    where
        extract :: Type -> String
        extract (StrType s) = s
        extract _ = error "Invalid usage of extract"



std_writeFile _ _ = throwError $ InvalidArguments

-- String
-- Concat StrArrayType to StrType
std_concat :: Function
std_concat [] _ = throwError $ NoArguments
std_concat ((StrArrayType xs) : []) _ = return $ StrType $ concat $ map extract xs
    where
        extract :: Type -> String
        extract (StrType s) = s
        extract _ = error "Invalid usage of extract"

std_concat ((StrArrayType xs) : (StrType sep) : []) _ = return $ StrType $ concat $ intersperse sep $ map extract xs
    where
        extract :: Type -> String
        extract (StrType s) = s
        extract _ = error "Invalid usage of extract"
std_concat _ _ = throwError $ InvalidArguments

-- Split StrType to StrArrayType
std_split :: Function
std_split [] _ = throwError $ NoArguments
std_split ((StrType s) : (StrType splt) : []) _
    | length splt >= length s = throwError $ CustomError "Splitting element longer than string"
    | splt == ""            = return $ StrArrayType [StrType s]
    | otherwise = return $ StrArrayType $ map StrType $ splitOn splt s
