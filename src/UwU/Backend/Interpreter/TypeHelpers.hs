module UwU.Backend.Interpreter.TypeHelpers where

import UwU.Backend.Interpreter.Runtime
import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Map as Map
import Control.Monad.Except
import Text.Read
import UwU.Frontend.AST (Stmt,Expr)

type I32 = Int
type Str = String

type Function = ([Type] -> SymbolTable -> Runtime(Type)) 

data Type = IntType I32 
    | IntArrayType [Type]
    | StrType String
    | StrArrayType [Type]
    | FType Function

instance Show Type where
    show (IntType i) = show i
    show (StrType s) = s
    show (StrArrayType ss) = unwords $ intersperse "," $ map show ss
    show (IntArrayType is) = unwords $ intersperse "," $ map show is
    show (FType _) = "Function"

cmpType :: Type -> Type -> Runtime(Ordering)
cmpType (IntType a) (IntType b) = return $ compare a b
cmpType (StrType a) (StrType b) = return $ compare a b
cmpType _ _ = throwError $ CustomError "[Error] Compare is not supported for arrays or mixed types"


safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:xs) 0 = Just x
safeIndex (x:xs) i = safeIndex xs (i-1)

safeUpdate :: [a] -> a -> Int -> Maybe [a]
safeUpdate [] _ _ = Nothing
safeUpdate (a:as) v 0 = Just (v : as)
safeUpdate (a:as) v i = safeUpdate as v (i-1) >>= (\xs -> return $ a:xs)


get :: Type -> Type -> Runtime(Type)
get (IntArrayType xs) (IntType i) = 
    case safeIndex xs i of
        Just (IntType x) -> return (IntType x)
        Nothing -> throwError $ IndexOutOfBounds i

get (StrArrayType xs) (IntType i) =
    case safeIndex xs i of
        Just (StrType x) -> return (StrType x)
        Nothing -> throwError $ IndexOutOfBounds i

get _ (IntType _ ) = throwError VariableNotAnArray
get _ _          = throwError NonIntIndex

put :: Type -> Type -> Type -> Runtime(Type)
put (IntArrayType xs) (IntType i) (IntType v) =
    case safeUpdate xs (IntType v) i of
        Just xs' -> return $ IntArrayType xs'
        Nothing -> throwError $ IndexOutOfBounds i

put (IntArrayType xs) (IntType i) (StrType v) =
    case readMaybe v :: Maybe Int of
        Just i -> put (IntArrayType xs) (IntType i) (IntType i)
        Nothing -> throwError $ CastToIntFail v
    
put (StrArrayType xs) (IntType i) (StrType v) =
    case safeUpdate xs (StrType v) i of
        Just xs' -> return $ StrArrayType xs'
        Nothing -> throwError $ IndexOutOfBounds i

put (StrArrayType xs) (IntType i) (IntType v) =
    put (StrArrayType xs) (IntType i) (StrType (show v))


put _ (IntType _ ) _ = throwError VariableNotAnArray
put _ _ _ = throwError NonIntIndex

typeShow :: Type -> Maybe String
typeShow (IntType v)      = Just $ show v
typeShow (IntArrayType vs) = Just $ unwords $ mapMaybe typeShow vs
typeShow (StrType v)      = Just v
typeShow (StrArrayType vs) = Just $ unwords $ mapMaybe typeShow vs
typeShow (FType _ )      = Nothing
 
type SymbolTable = Map.Map String Type
