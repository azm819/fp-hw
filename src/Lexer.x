{
module Lexer ( runAlexWithPath
             , alexMonadScanWithPos
             , alexErrorWithPos
             , Alex(..)
             , Token(..)
             , AToken(..)) where

import Control.Monad (when)
}

%wrapper "monadUserState"

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

pass           { tok_ TPass         }
and            { tok_ TAnd          }
or             { tok_ TOr           }
not            { tok_ TNot          }
if             { tok_ TIf           }
else           { tok_ TElse         }
elif           { tok_ TElif         }
while          { tok_ TWhile        }
return         { tok_ TReturn       }
def            { tok_ TDef          }
\(             { tok_ TLParen       }
\)             { tok_ TRParen       }
\:             { tok_ TColon        }
\,             { tok_ TComma        }
\+             { tok_ TPlus         }
\-             { tok_ TMinus        }
\*             { tok_ TStar         }
\/             { tok_ TSlash        }
\<             { tok_ TLess         }
\>             { tok_ TGreater      }
\=             { tok_ TEqual        }
\%             { tok_ TPercent      }
\=\=           { tok_ TEqEqual      }
\!\=           { tok_ TNotEqual     }
\<\=           { tok_ TLessEqual    }
\>\=           { tok_ TGreaterEqual }

$digit+        { tok (TInt . read)        }
\" $notdq+ \"  { tok (TStr . tail . init) }
$alpha $alnum* { tok TName                }

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

-- Token, annotated with alex position
data AToken = AToken AlexPosn Token
  deriving (Eq, Show)

data AlexUserState = AlexUserState { filePath :: FilePath
                                   , indents  :: [Int]
                                   , pending  :: [AToken]
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" [1] []

getFilePath :: Alex FilePath
getFilePath = filePath <$> alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath p = do
  st <- alexGetUserState
  alexSetUserState $ st { filePath = p }

getLastIndent :: Alex Int
getLastIndent = (head . indents) <$> alexGetUserState

indent :: Int -> Alex ()
indent i = do
  st <- alexGetUserState
  alexSetUserState $ st { indents = i:indents st }

dedent :: Alex ()
dedent = do
  st <- alexGetUserState
  alexSetUserState $ st { indents = tail (indents st) }

consumePending :: Alex (Maybe AToken)
consumePending = do
  st <- alexGetUserState
  case pending st of
    [] -> return Nothing
    x:xs -> do
      alexSetUserState $ st { pending = xs }
      return $ Just x

emit :: AlexPosn -> Token -> Alex ()
emit posn token = do
  st <- alexGetUserState
  alexSetUserState $ st { pending = (pending st) <> [AToken posn token] }

getAlexPosn :: Alex AlexPosn
getAlexPosn = do
  (posn, _, _, _) <- alexGetInput
  return posn

alexEOF :: Alex AToken
alexEOF = do
  posn <- getAlexPosn
  return $ AToken posn TEOF

tok :: (String -> Token) -> AlexAction AToken
tok f = \(posn, _, _, s) i -> return $ AToken posn $ f $ take i s

tok_ :: Token -> AlexAction AToken
tok_ = tok . const

startWhite :: AlexAction AToken
startWhite (posn, _, _, _) cur = do
  prev <- getLastIndent
  when (cur > prev) $ do
    indent cur
    emit posn TIndent
  when (cur < prev) $ do
    dedent
    prev' <- getLastIndent
    when (prev' /= cur) $ do
      alexErrorWithPos posn "Invalid dedent"
    emit posn TDedent
  return $ AToken posn TNewline

alexMonadScanWithPos :: Alex AToken
alexMonadScanWithPos = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  p <- consumePending
  case p of
    Just t -> return t
    Nothing -> 
      case alexScan inp sc of
        AlexEOF -> alexEOF
        AlexError (posn, _, _, s) ->
            alexErrorWithPos posn ("lexical error at character '" <> take 1 s <> "'")
        AlexSkip  inp' len -> do
            alexSetInput inp'
            alexMonadScanWithPos
        AlexToken inp' len action -> do
            alexSetInput inp'
            action (ignorePendingBytes inp) len

alexErrorWithPos :: AlexPosn -> String -> Alex a
alexErrorWithPos (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp <> ":" <> show l <> ":" <> show c <> ": " <> msg)

runAlexWithPath :: Alex a -> FilePath -> String -> Either String a
runAlexWithPath a fp input = runAlex input (setFilePath fp >> a)
}
