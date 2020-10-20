{
{-# Language FlexibleContexts #-}
module Parser (parse) where

import Lexer
import AST

import Control.Monad.Except
}

%name parse
%tokentype { AToken }
%error { parseError }

%token
  eof             { AToken TEOF          _ }
  pass            { AToken TPass         _ }
  newline         { AToken TNewline      _ }
  indent          { AToken TIndent       _ }
  dedent          { AToken TDedent       _ }
  name            { AToken (TName $$)    _ }
  int             { AToken (TInt $$)     _ }
  str             { AToken (TStr $$)     _ }
  and             { AToken TAnd          _ }
  or              { AToken TOr           _ }
  not             { AToken TNot          _ }
  if              { AToken TIf           _ }
  else            { AToken TElse         _ }
  elif            { AToken TElif         _ }
  while           { AToken TWhile        _ }
  return          { AToken TReturn       _ }
  def             { AToken TDef          _ }
  '('             { AToken TLParen       _ }
  ')'             { AToken TRParen       _ }
  ':'             { AToken TColon        _ }
  ','             { AToken TComma        _ }
  '+'             { AToken TPlus         _ }
  '-'             { AToken TMinus        _ }
  '*'             { AToken TStar         _ }
  '/'             { AToken TSlash        _ }
  '<'             { AToken TLess         _ }
  '>'             { AToken TGreater      _ }
  '='             { AToken TEqual        _ }
  '%'             { AToken TPercent      _ }
  '=='            { AToken TEqEqual      _ }
  '!='            { AToken TNotEqual     _ }
  '<='            { AToken TLessEqual    _ }
  '>='            { AToken TGreaterEqual _ }

%nonassoc '==' '!=' '<' '<=' '>' '>='
%left or
%left and
%left not
%left '+' '-'
%left '*' '/' '%'
%left NEG
%%

Program : Statements eof { $1 }

Statements : { [] }
           | Statement Statements { $1 : $2 }
           | newline Statements { $2 }

Exp : Exp '=='  Exp { Equal $1 $3 }
    | Exp '!='  Exp { NotEqual $1 $3 }
    | Exp '<'   Exp { Less $1 $3 }
    | Exp '>'   Exp { Greater $1 $3 }
    | Exp '<='  Exp { LessEqual $1 $3 }
    | Exp '>='  Exp { GreaterEqual $1 $3 }
    | Exp or    Exp { Or $1 $3 }
    | Exp and   Exp { And $1 $3 }
    | not       Exp { Not $2 }
    | Exp '+'   Exp { Add $1 $3 }
    | Exp '-'   Exp { Sub $1 $3 }
    | Exp '*'   Exp { Mul $1 $3 }
    | Exp '/'   Exp { Div $1 $3 }
    | Exp '%'   Exp { Mod $1 $3 }
    | '-' Exp %prec NEG { Neg $2 }
    | '(' Exp ')' { $2 }
    | name '(' ExpList ')' { Call $1 $3 }
    | name { Name $1 }
    | int { IntLiteral $1 }
    | str { StrLiteral $1 }

ExpList : { [] }
        | Exp { [$1] }
        | Exp ',' ExpList { $1 : $3 }

NameList : { [] }
         | name { [$1] }
         | name ',' NameList { $1 : $3 }

Block : ':' newline indent Body dedent { $4 }

Body : pass newline { [] }
     | Statements { $1 }

Statement : Exp newline { Exp $1 }
          | If { $1 }
          | Return newline { $1 }
          | While { $1 }
          | Def { $1 }
          | Assn newline { $1 }

IfBody : Exp Block Else { If $1 $2 $3 }

If : if IfBody { $2 }

Else : { Nothing }
     | else Block { Just $2 }
     | elif IfBody { Just [$2] }

Return : return Exp { Return $2 }

While : while Exp Block { While $2 $3 }

Def : def name '(' NameList ')' Block { Def $2 $4 $6 }

Assn : name '=' Exp { Assn $1 $3 }

{
parseError ((AToken tok line):_) = error $ "Unexpected " <> (show tok) <> " at line " <> (show line)
}
