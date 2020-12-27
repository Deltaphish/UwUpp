module UwU.Frontend.AST where

type Name = String
type Value = Int
type Index = Int

data ArrayType = ArrayTypeString | ArrayTypeInt deriving (Eq, Ord,Show)

data Stmt
   = Assign Name Expr
   |  AssignIndex Expr Expr
   |  Function Name [Name] [Stmt] Expr
   |  If Cond [Stmt]
   |  While Cond [Stmt]
   |  Print Expr
   |  PrintStr String
   |  InitArray Name Expr ArrayType
   |  FunctionCall Name [Expr]
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
  | Str String
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