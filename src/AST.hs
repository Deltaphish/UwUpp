module AST where

type Name = String
type Value = Int

data Stmt
   = Assign Name Expr
   |  AssignIndex Expr Expr
   |  Function Name [Name] [Stmt] Expr
   |  If Cond [Stmt]
   |  While Cond [Stmt]
   |  Print Expr
   |  PrintStr String
   |  InitArray Name Expr
   deriving (Eq, Ord, Show)

data Cond
   = Great Expr Expr
   |  Less Expr Expr
   |  Equal Expr Expr 
   deriving (Eq, Ord,Show)

data Expr
  = Var Name
  | Index Name Expr
  | Call Name [Expr]
  | Int Value
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)