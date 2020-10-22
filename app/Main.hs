module Main where

import AST
import Parser
import Runner
import System.Environment
import System.IO

action :: Statements -> [String] -> IO ()
action prog [] = run prog
action prog ["-p"] = putStr $ showStatements prog

main :: IO ()
main = do
  (filename:rest) <- getArgs
  s <- readFile filename
  case parse filename s of
    Left err -> putStrLn err
    Right prog -> action prog rest
