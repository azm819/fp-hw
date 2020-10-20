module Lib ( someFunc ) where

import Lexer
import Parser

someFunc :: IO ()
someFunc = do
  s <- getContents
  print $ parse $ tokenize s
