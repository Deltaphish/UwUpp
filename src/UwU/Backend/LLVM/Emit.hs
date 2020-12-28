{-# LANGUAGE OverloadedStrings #-}
module UwU.Backend.LLVM.Emit where

import LLVM.AST as AST
import LLVM.Module
import LLVM.Context 
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as B
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as Ch8
import qualified Data.ByteString.Short as B.Short
import UwU.Frontend.AST as UwU.AST
import UwU.Backend.LLVM.IRGen as IRGen
import qualified LLVM.AST.Type as Type
import LLVM.AST.AddrSpace


import Control.Monad.Except

import Control.Monad

stosbs :: String -> B.Short.ShortByteString
stosbs s = B.Short.toShort $ toStrict $  toLazyByteString b
    where
        b :: Builder
        b = stringUtf8 s

{-
codegenTop :: UwU.AST.Stmt -> LLVM ()
codegenTop (UwU.AST.Function name args body ret) = do
  define IRGen.intType (stosbs name) fnargs bls
  where
    fnargs = toSig (map stosbs args)
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM_ (map stosbs args) $ \a -> do
        var <- alloca intType
        store var (local (AST.Name a))
        assign a var
      mapM_ codegenTop body >>= ret
-}

cFunctions :: UwU.AST.Stmt -> Definition 
cFunctions (UwU.AST.Function nm args body retEx) = do
    define (Type.PointerType intType (AddrSpace 0)) (stosbs nm) targs bd
    where
      targs = toSig $ map stosbs args
      bd = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM_ (map stosbs args) $ \a -> do
          var <- alloca intType
          store var (local (AST.Name a))
          assign a var
        mapM_ cStmt body
        retVal <- cgen retEx
        ret retVal

toSig :: [Str] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (IRGen.intType, AST.Name x))

cStmt :: UwU.AST.Stmt -> Codegen ()
cStmt (UwU.AST.Assign nm exp) = do
  val <- cgen exp
  assign (stosbs nm) val
 
 {-
cStmt stmts = do
  define IRGen.intType "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cStmt stmts
      c <- getvar "ret"
      ret c

-}

cgen :: UwU.AST.Expr -> Codegen AST.Operand
cgen (UwU.AST.Int n) = return $ cons $ C.Int 64 (toInteger  n)
cgen (UwU.AST.Var x) = getvar (stosbs x)
cgen (UwU.AST.Sum x1 x2) = do
  op1 <- cgen x1
  op2 <- cgen x2
  fadd op1 op2

cgen (UwU.AST.Subtr x1 x2) = do
  op1 <- cgen x1
  op2 <- cgen x2
  fsub op1 op2

cgen (UwU.AST.Product x1 x2) = do
  op1 <- cgen x1
  op2 <- cgen x2
  fmul op1 op2

cgen (UwU.AST.Division x1 x2) = do
  op1 <- cgen x1
  op2 <- cgen x2
  fdiv op1 op2

cgen (UwU.AST.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name (stosbs fn))) largs

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [UwU.AST.Stmt] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    Ch8.putStrLn llstr
    return newast
  where
    newast = runLLVM mod modn'
    fnstmts = mapM_ addDefn $ map cFunctions $ filter isFn fns
    modn' = modn >> fnstmts
    modn = addDefn $ define intType "main" [] $ createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        (mapM cStmt $ filter (not.isFn) fns) >> (ret $ cons $ C.Int 64 0 )

isFn :: UwU.AST.Stmt -> Bool
isFn (UwU.AST.Function _ _ _ _) = True
isFn _ = False