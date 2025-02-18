{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}

module AST where

import Control.Monad.Free ( liftF, Free(..) )

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
         deriving Eq

data Statement = Exp Exp
               | If Exp Statements Statements
               | Return Exp
               | While Exp Statements
               | Def String [String] Statements
               | Assn String Exp
               deriving Eq

type Statements = [Statement]

data Cmd a = EExp Exp a
           | EIf Exp EDSL a
           | EElse EDSL a
           | EElif Exp EDSL a
           | EWhile Exp EDSL a
           | EDef String [String] EDSL a
           | EReturn Exp a
           | EAssn String Exp a
           deriving Functor

type EDSL = Free Cmd ()

collapseElif :: EDSL -> (Statements, EDSL)
collapseElif (Free (EElse body next)) = (edsl body, next)
collapseElif (Free (EElif exp body next)) = ([If exp (edsl body) body'], next')
  where (body', next') = collapseElif next
collapseElif x = ([], x)

edsl :: EDSL -> Statements
edsl (Pure ()) = []
edsl (Free (EExp exp next)) = Exp exp : edsl next
edsl (Free (EIf exp body next)) = If exp (edsl body) body' : edsl next'
  where (body', next') = collapseElif next
edsl (Free (EWhile exp body next)) = While exp (edsl body) : edsl next
edsl (Free (EDef name args body next)) = Def name args (edsl body) : edsl next
edsl (Free (EAssn name exp next)) = Assn name exp : edsl next
edsl (Free (EReturn exp next)) = Return exp : edsl next

pass :: EDSL
pass = pure ()

n :: String -> Exp
n = Name

i :: Int -> Exp
i = IntLiteral

s :: String -> Exp
s = StrLiteral

e :: Exp -> EDSL
e exp = liftF (EExp exp ())

if' :: Exp -> EDSL -> EDSL
if' exp body = liftF (EIf exp body ())

else' :: EDSL -> EDSL
else' body = liftF (EElse body ())

elif :: Exp -> EDSL -> EDSL
elif exp body = liftF (EElif exp body ())

return' :: Exp -> EDSL
return' exp = liftF (EReturn exp ())

while :: Exp -> EDSL -> EDSL
while exp body = liftF (EWhile exp body ())

def :: String -> [String] -> EDSL -> EDSL
def name args body = liftF (EDef name args body ())

infix 2 $=$
infixl 3 `or'`
infixl 4 `and'`
infix 5 $==$, $!=$, $<$, $<=$, $>$, $>=$
infixl 6 $+$, $-$
infixl 7 $*$, $/$, $%$
infixl 8 $$

neg' :: Exp -> Exp
neg' = Neg

not' :: Exp -> Exp
not' = Not

or' :: Exp -> Exp -> Exp
or' = Or

and' :: Exp -> Exp -> Exp
and' = And

($=$) :: String -> Exp -> EDSL
name $=$ exp = liftF (EAssn name exp ())

($==$) :: Exp -> Exp -> Exp
($==$) = Equal

($!=$) :: Exp -> Exp -> Exp
($!=$) = NotEqual

($<$) :: Exp -> Exp -> Exp
($<$) = Less

($>$) :: Exp -> Exp -> Exp
($>$) = Greater

($<=$) :: Exp -> Exp -> Exp
($<=$) = LessEqual

($>=$) :: Exp -> Exp -> Exp
($>=$) = GreaterEqual

($+$) :: Exp -> Exp -> Exp
($+$) = Add

($-$) :: Exp -> Exp -> Exp
($-$) = Sub

($*$) :: Exp -> Exp -> Exp
($*$) = Mul

($/$) :: Exp -> Exp -> Exp
($/$) = Div

($%$) :: Exp -> Exp -> Exp
($%$) = Mod

($$) :: String -> [Exp] -> Exp
($$) = Call

-------------
-- Printer --
-------------


instance Show Exp where
  showsPrec p exp = case exp of
    Name name        -> showString name
    IntLiteral i     -> shows i
    StrLiteral s     -> shows s
    Equal a b        -> binop 6 a b "=="
    NotEqual a b     -> binop 6 a b "!="
    Less a b         -> binop 6 a b "<"
    Greater a b      -> binop 6 a b ">"
    LessEqual a b    -> binop 6 a b "<="
    GreaterEqual a b -> binop 6 a b ">="
    Or a b           -> binop 3 a b "or"
    And a b          -> binop 4 a b "and"
    Not a            -> showParen (p >= 5) $ showString "not " . showsPrec 5 a
    Add a b          -> binop 7 a b "+"
    Sub a b          -> binop 7 a b "-"
    Mul a b          -> binop 8 a b "*"
    Div a b          -> binop 8 a b "/"
    Mod a b          -> binop 8 a b "%"
    Neg a            -> showParen (p >= 9) $ showString "-" . showsPrec 9 a
    Call name args   -> showParen (p >= 10) $ showString name . shows args
    where binop prec a b symb = showParen (p >= prec) $ showsPrec prec a . showString (" " <> symb <> " ") . showsPrec prec b
  showList xs = showParen True $ showxs xs
    where showxs [] = showString ""
          showxs [y] = shows y
          showxs (y:ys) = shows y . showString ", " . showxs ys

instance Show Statement where
  showsPrec _ (Exp exp) = shows exp
  showsPrec _ (If exp body []) = showString "if " . shows exp . indented body
  showsPrec _ (If exp body1 body2) = shows (If exp body1 []) . showString "else" . indented body2
  showsPrec _ (Return exp) = showString "return " . shows exp
  showsPrec _ (While exp body) = showString "while " . shows exp . indented body
  showsPrec _ (Def name args body) = showString "def " . showString name . shows (map Name args) . indented body
  showsPrec _ (Assn name exp) = showString name . showString " = " . shows exp

showStatements :: Statements -> String
showStatements [] = "pass"
showStatements [x] = show x
showStatements (x:xs) = show x <> "\n" <> showStatements xs

indented :: Statements -> ShowS
indented statements s = ":\n" <> (unlines $ map ("  " <> ) (lines (showStatements statements))) <> s
