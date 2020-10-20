{
module Lexer (tokenize, Token(..), AToken(..)) where

import Data.Word
import Control.Monad.State
import Codec.Binary.UTF8.String
}

$digit  = [0-9]
$alpha  = [a-zA-Z]
$alnum  = [$alpha$digit]
$space  = [\ \t]
$eol    = [\n\r]
$noteol = ~$eol
$notdq  = ~[\"]

@ident = $alpha $alnum*

tokens :-

\# $noteol* ;
$eol$space*    { startWhite        }
$space+ ;

pass           { tok TPass         }
and            { tok TAnd          }
or             { tok TOr           }
not            { tok TNot          }
if             { tok TIf           }
else           { tok TElse         }
elif           { tok TElif         }
while          { tok TWhile        }
return         { tok TReturn       }
def            { tok TDef          }
\(             { tok TLParen       }
\)             { tok TRParen       }
\:             { tok TColon        }
\,             { tok TComma        }
\+             { tok TPlus         }
\-             { tok TMinus        }
\*             { tok TStar         }
\/             { tok TSlash        }
\<             { tok TLess         }
\>             { tok TGreater      }
\=             { tok TEqual        }
\%             { tok TPercent      }
\=\=           { tok TEqEqual      }
\!\=           { tok TNotEqual     }
\<\=           { tok TLessEqual    }
\>\=           { tok TGreaterEqual }

$digit+        { intToken          }
\" $notdq+ \"  { strToken          }
$alpha $alnum* { nameToken         }

{
data Token = TIndent
           | TDedent
           | TNewline
           | TName String
           | TInt Int
           | TStr String
           | TLParen
           | TRParen
           | TColon
           | TComma
           | TPlus
           | TMinus
           | TStar
           | TSlash
           | TLess
           | TGreater
           | TEqual
           | TPercent
           | TEqEqual
           | TNotEqual
           | TLessEqual
           | TGreaterEqual
           | TPass
           | TAnd
           | TOr
           | TNot
           | TIf
           | TElse
           | TElif
           | TDef
           | TReturn
           | TWhile
           | TEOF
           deriving (Eq, Show)

data AToken = AToken Token Int
  deriving (Eq, Show)

data AlexInput = AlexInput Char [Word8] String
  deriving (Show, Eq)
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput c (b:bs) s) = Just (b,AlexInput c bs s)
alexGetByte (AlexInput c [] [])    = Nothing
alexGetByte (AlexInput _ [] (c:s)) =
  case encode [c] of
    (b:bs) -> Just (b, AlexInput c bs s)
    _      -> Nothing
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput c _ _) = c

data AlexState = AlexState { input :: AlexInput
                           , indents :: [Int]
                           , pending :: [AToken]
                           , line :: Int
                           }
                           deriving Show

initialState :: String -> AlexState
initialState s = AlexState { input = AlexInput '\n' [] s
                           , indents = [1]
                           , pending = []
                           , line = 1
                           }

type Lexer = State AlexState

token :: Token -> Lexer AToken
token t = do
  s <- get
  return $ AToken t (line s)

evalLexer :: Lexer a -> String -> a
evalLexer l s = evalState l (initialState s)

intToken :: Int -> String -> Lexer AToken
intToken _ s = token $ TInt $ read s

strToken :: Int -> String -> Lexer AToken
strToken _ s = token $ TStr $ init $ tail s

nameToken :: Int -> String -> Lexer AToken
nameToken _ s = token $ TName s

tok :: Token -> Int -> String -> Lexer AToken
tok t _ _ = token t

startWhite :: Int -> String -> Lexer AToken
startWhite n _ = do
  s' <- get
  let s = s' { line = line s + 1 }
  put s
  let l = line s
      i@(cur:_) = indents s
  when (n > cur) $ do
    put s { indents = n:i, pending = [AToken TIndent l] }
  when (n < cur) $ do
    let (pre, post@(top:_)) = span (> n) i
    if top == n then put s { indents = post, pending = map (const (AToken TDedent l)) pre }
                else error "Indents don't match"
  return $ AToken TNewline l

readToken :: Lexer AToken
readToken = do
  s <- get
  case pending s of
    x:xs -> do -- consume pending tokens
      put s { pending = xs }
      return x
    _ -> case alexScan (input s) 0 of
           AlexEOF -> do
             ret <- startWhite 1 ""
             put s { pending = (pending s) <> [AToken TEOF (line s)] }
             return ret
           AlexError _ -> error "Lexical error"
           AlexSkip inp _ -> do
             put s { input = inp }
             readToken
           AlexToken inp n act -> do
             let (AlexInput _ _ buf) = input s
             put s { input = inp }
             act n (take n buf)

readTokens :: Lexer [AToken]
readTokens = do
  token <- readToken
  case token of
    (AToken TEOF _) -> return [token]
    _ -> do
      rest <- readTokens
      return (token:rest)

tokenize :: String -> [AToken]
tokenize = evalLexer readTokens
}
