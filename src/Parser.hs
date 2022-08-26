{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import qualified Text.Parser.Combinators as Comb
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Control.Monad
import Data.Text
import Data.Void

import AST

type Parser = Parsec Void Text

sc :: Parser()
sc = L.space
  space1
  (L.skipLineComment "UwU")
  (L.skipBlockComment "( ͡° ͜ʖ ͡°)" "( ͡° ͜ʖ ͡°)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

--stringLiteral :: Parser String
--stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pInt :: Parser Expr
pInt = Int <$> lexeme L.decimal

identifier :: Parser String
identifier = lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pVar = Var <$> identifier

pIndex =
   do name <- identifier
      symbol "["
      index <- pExpr
      symbol "]"
      return $ Index name index

pFcallStmt :: Parser Stmt
pFcallStmt = do
    fname <- identifier
    symbol "("
    args <- pExpr `Comb.sepBy` (symbol ",")
    symbol ")"
    return $ FunctionCall fname args

pFcall :: Parser Expr
pFcall = do 
    fname <- identifier
    symbol "("
    args <- pExpr `Comb.sepBy` (symbol ",")
    symbol ")"
    return $ Call fname args

pTerm :: Parser Expr
pTerm = pInt <|> try pStr <|> try pFcall <|> try pIndex <|> pVar

pAssign :: Parser Stmt
pAssign = 
   do varName <- identifier
      void <- (symbol "iws")
      expr <- pExpr
      return (Assign varName expr)

pAssignIndex :: Parser Stmt
pAssignIndex = 
   do index <- pIndex
      symbol "iws"
      expr <- pExpr
      return $ AssignIndex index expr

pInitArray :: Parser Stmt
pInitArray = 
   do varName <- identifier
      symbol "iws"
      symbol "awway<"
      len <- pTerm
      sc
      symbol ";"
      t <- choice [
         ArrayTypeInt <$ symbol "int",
         ArrayTypeString <$ symbol "str"]

      symbol ">"
      return $ InitArray varName len t


pfunction :: Parser Stmt
pfunction =
   do symbol "nyaa"
      symbol "*"
      fname <- identifier
      args <- parens $ identifier `sepBy` (symbol ",")
      symbol "*"
      body <- try $ pStmt `manyTill` (symbol "wetuwn")
      ret <- pExpr
      return $ Function fname args body ret

pCond :: Parser Cond
pCond =
   do expr1 <- pExpr
      comp <- choice 
                [ Great <$ symbol "gweatew twan" 
                , Less <$ symbol "wess twan"
                , Equal <$ symbol "eqwall twoo"
                ]
      expr2 <- pExpr
      return $ comp expr1 expr2

pIf :: Parser Stmt
pIf =
   do void <- (symbol "*notices")
      cond <- pCond
      void <- symbol "*\n"
      body <- try $ pStmt `manyTill` (symbol "stawp")
      return $ If cond body

pWhile :: Parser Stmt
pWhile =
   do void <- (symbol "OwO *notices")
      cond <- pCond
      symbol "*\n"
      body <- try $ pStmt `manyTill` (symbol "stawp")
      return $ While cond body

{-
pPrint :: Parser Stmt
pPrint = 
   do symbol "nuzzels "
      expr <- pExpr
      return $ Print expr
-}

pString :: Parser String
pString = choice [
                  char '"' >> manyTill L.charLiteral (char '"'),
                  char '\'' >> manyTill L.charLiteral (char '\'')]

pStr :: Parser Expr
pStr = do
   s <- pString
   sc
   return $ Str s

{-
pPrintStr :: Parser Stmt
pPrintStr =
   do symbol "nuzzels "
      str <- pString
      return $ PrintStr str
-}

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pStmt :: Parser Stmt
pStmt = do try pfunction <|> try pWhile <|> try pIf <|> try pAssignIndex <|> try pInitArray <|> try pAssign <|> pFcallStmt

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "pwus" id
    ]
  , [ binary "twimes" Product
    , binary "diwide" Division
    , binary "wemaindew" Rem
    ]
  , [ binary "pwus" Sum
    , binary "minwus" Subtr
    ]]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)


pMain = do statements <- many (sc *> pStmt)
           return statements
