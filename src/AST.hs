{-# Language DeriveFunctor #-}

module AST where

import Control.Monad.Free

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

data Cmd a = EExp Exp a
           | EIf Exp EDSL a
           | EElse EDSL a
           | EElif Exp EDSL a
           | EWhile Exp EDSL a
           | EDef String [String] EDSL a
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

while :: Exp -> EDSL -> EDSL
while exp body = liftF (EWhile exp body ())

def :: String -> [String] -> EDSL -> EDSL
def name args body = liftF (EDef name args body ())

infix 2 $=$
infix 3 $==$, $!=$, $<$, $<=$, $>$, $>=$
infixl 4 `or'`
infixl 5 `and'`
infixl 6 $+$, $-$
infixl 7 $*$, $/$, $%$
infixl 8 $$

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
