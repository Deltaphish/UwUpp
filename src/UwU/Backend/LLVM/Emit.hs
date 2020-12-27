{-# LANGUAGE OverloadedStrings#-}
module UwU.Backend.LLVM.Emit where

import LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as B
import Data.ByteString.Builder
import qualified Data.ByteString.Short as B.Short
import UwU.Frontend.AST as UwU.AST
import UwU.Backend.LLVM.IRGen as IRGen

stosbs :: String -> B.Short.ShortByteString
stosbs s = B.Short.toShort $ toStrict $  toLazyByteString b
    where
        b :: Builder
        b = stringUtf8 s


codegenTop :: UwU.AST.Stmt -> LLVM ()
codegenTop (UwU.AST.Function name args body ret) = do
  define IRGen.intType name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret

codegenTop exp = do
  define IRGen.intType "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

toSig :: [Str] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (IRGen.intType, AST.Name x))

cgen :: UwU.AST.Expr -> Codegen AST.Operand
cgen (UwU.AST.Int n) = return $ cons $ C.Int 64 (toInteger  n)
cgen (UwU.AST.Var x) = getvar x >>= load

cgen (UwU.AST.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name (stosbs fn))) largs

