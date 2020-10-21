{
{-# Language FlexibleContexts #-}
module Parser (parse) where

import Lexer
import AST

import Control.Monad.Except
}

%name parsex
%tokentype { AToken }
%monad { Alex }
%lexer { lexer } { AToken _ TEOF }
%error { happyError }

%token
  eof             { AToken _ TEOF          }
  pass            { AToken _ TPass         }
  newline         { AToken _ TNewline      }
  indent          { AToken _ TIndent       }
  dedent          { AToken _ TDedent       }
  name            { AToken _ (TName $$)    }
  int             { AToken _ (TInt $$)     }
  str             { AToken _ (TStr $$)     }
  and             { AToken _ TAnd          }
  or              { AToken _ TOr           }
  not             { AToken _ TNot          }
  if              { AToken _ TIf           }
  else            { AToken _ TElse         }
  elif            { AToken _ TElif         }
  while           { AToken _ TWhile        }
  return          { AToken _ TReturn       }
  def             { AToken _ TDef          }
  '('             { AToken _ TLParen       }
  ')'             { AToken _ TRParen       }
  ':'             { AToken _ TColon        }
  ','             { AToken _ TComma        }
  '+'             { AToken _ TPlus         }
  '-'             { AToken _ TMinus        }
  '*'             { AToken _ TStar         }
  '/'             { AToken _ TSlash        }
  '<'             { AToken _ TLess         }
  '>'             { AToken _ TGreater      }
  '='             { AToken _ TEqual        }
  '%'             { AToken _ TPercent      }
  '=='            { AToken _ TEqEqual      }
  '!='            { AToken _ TNotEqual     }
  '<='            { AToken _ TLessEqual    }
  '>='            { AToken _ TGreaterEqual }

%left or
%left and
%left not
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%left NEG
%%

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

Else : { [] }
     | else Block { $2 }
     | elif IfBody { [$2] }

Return : return Exp { Return $2 }

While : while Exp Block { While $2 $3 }

Def : def name '(' NameList ')' Block { Def $2 $4 $6 }

Assn : name '=' Exp { Assn $1 $3 }


{
lexer :: (AToken -> Alex a) -> Alex a
lexer = (alexMonadScanWithPos >>=)

happyError :: AToken -> Alex a
happyError (AToken p t) =
  alexErrorWithPos p ("parse error at token " <> show t)

parse :: FilePath -> String -> Either String Statements
parse = runAlexWithPath parsex
}
