module AST where

data Exp = Name String
         | IntLiteral Int
         | StrLiteral String
         | Equal Exp Exp
         | NotEqual Exp Exp
         | Less Exp Exp
         | Greater Exp Exp
         | LessEqual Exp Exp
         | GreaterEqual Exp Exp
         | Or Exp Exp
         | And Exp Exp
         | Not Exp
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Neg Exp
         | Call String [Exp]
         deriving (Eq, Show)

data Statement = Exp Exp
               | If Exp Statements Statements
               | Return Exp
               | While Exp Statements
               | Def String [String] Statements
               | Assn String Exp
               deriving (Eq, Show)

type Statements = [Statement]
