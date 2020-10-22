{-# Language RankNTypes #-}

module Runner (run) where

import AST
import Text.Read (readMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.Foldable
import System.IO.Error

data Val = Int Int
         | Str String
         | Bool Bool
         | None
         | Fun PyFun

instance Show Val where
  show (Int i) = show i
  show (Str s) = s
  show (Bool b) = show b
  show None = "None"
  show (Fun _) = "<Function>"

instance Eq Val where
  (Int a) == (Int b) = a == b
  (Str a) == (Str b) = a == b
  (Bool a) == (Bool b) = a == b
  None == None = True
  _ == _ = False

toBool :: Val -> Bool
toBool (Int 0) = False
toBool (Str "") = False
toBool (Bool False) = False
toBool None = False
toBool _ = True

type Scope = Map String Val

type Runner = StateT Scope IO

type PyFun = [Val] -> Runner Val

data PyErr = VariableNotInScope String
           | InvalidLiteral String
           | WrongNumberOfArguments String
           | AssertionFailed
           | UnsupportedOperandTypes

pyErrToString :: PyErr -> String
pyErrToString (VariableNotInScope s) = "variable not in scope: " <> s
pyErrToString (InvalidLiteral s) = "invalid literal: " <> (show s)
pyErrToString (WrongNumberOfArguments s) = "wrong number of arguments for function " <> s <> "()"
pyErrToString AssertionFailed = "assertion failed"
pyErrToString UnsupportedOperandTypes = "unsupported operand types"

-- Brings a variable into scope
declare :: String -> Val -> Runner ()
declare name val = modify (M.insert name val)

-- Access a variable from scope, or throw an error
access :: String -> Runner Val
access name = do
  v <- gets $ M.lookup name
  case v of
    Just val -> return val
    Nothing  -> pyErr $ VariableNotInScope name

-- Throw an error
pyErr :: PyErr -> Runner a
pyErr = liftIO . ioError . userError . pyErrToString

-- Builtin global Python functions and constants:

pyStr :: PyFun
pyStr [] = return $ Str ""
pyStr [x] = return $ Str $ show x
pyStr _ = pyErr $ WrongNumberOfArguments "str"

pyInt :: PyFun
pyInt [] = return $ Int 0
pyInt [x] = case readMaybe (show x) of
              Just i -> return $ Int i
              Nothing -> pyErr $ InvalidLiteral (show x)
pyInt _ = pyErr $ WrongNumberOfArguments "int"

pyBool :: PyFun
pyBool [] = return $ Bool False
pyBool [x] = return $ Bool $ toBool x
pyBool _ = pyErr $ WrongNumberOfArguments "bool"

pyPrint :: PyFun
pyPrint args = do
  liftIO $ putStrLn $ unwords (map show args)
  return None

pyInput :: PyFun
pyInput [] = Str <$> liftIO getLine
pyInput [prompt] = liftIO (putStr (show prompt)) >> pyInput []
pyInput _ = pyErr $ WrongNumberOfArguments "input"

pyAssert :: PyFun
pyAssert [x] = if toBool x then return None
                           else pyErr AssertionFailed
pyAssert _ = pyErr $ WrongNumberOfArguments "assert"

-- Python builtins
globals :: Scope
globals = M.fromList [ ("True", Bool True)
                     , ("False", Bool False)
                     , ("None", None)
                     , ("str", Fun pyStr)
                     , ("int", Fun pyInt)
                     , ("bool", Fun pyBool)
                     , ("print", Fun pyPrint)
                     , ("input", Fun pyInput)
                     , ("assert", Fun pyAssert)
                     ]

-- Short circuit if a block returned a value
continue :: Runner (Maybe Val) -> Runner (Maybe Val) -> Runner (Maybe Val)
continue r1 r2 = do
  x <- r1
  case x of
    Nothing -> r2
    Just _ -> return x

-- Helper functions for expression evaluator:

evalEq :: Exp -> Exp -> Runner Bool
evalEq expA expB = do
  a <- runExp expA
  b <- runExp expB
  return $ case (a, b) of
    (Int x, Int y) -> x == y
    (Str x, Str y) -> x == y
    (Bool x, Bool y) -> x == y
    (None, None) -> True
    _ -> False

evalOrder :: Exp -> Exp -> (forall a. Ord a => a -> a -> Bool) -> Runner Val
evalOrder expA expB order = do
  a <- runExp expA
  b <- runExp expB
  case (a, b) of
    (Int x, Int y) -> f x y
    (Str x, Str y) -> f x y
    (Bool x, Bool y) -> f x y
    _ -> pyErr $ UnsupportedOperandTypes
  where f x y = return $ Bool $ x `order` y

evalIntOp :: Exp -> Exp -> (Int -> Int -> Int) -> Runner Val
evalIntOp expA expB op = do
  a <- runExp expA
  b <- runExp expB
  case (a, b) of
    (Int x, Int y) -> return $ Int $ op x y
    _ -> pyErr $ UnsupportedOperandTypes

-- Evaluate expression
runExp :: Exp -> Runner Val
runExp exp =
  case exp of
    Name name -> access name
    IntLiteral i -> return $ Int i
    StrLiteral s -> return $ Str s
    Equal a b -> Bool <$> evalEq a b
    NotEqual a b -> (Bool . not) <$> evalEq a b
    Less a b -> evalOrder a b (<)
    Greater a b -> evalOrder a b (>)
    LessEqual a b -> evalOrder a b (<=)
    GreaterEqual a b -> evalOrder a b (>=)
    Or a b -> do
      x <- runExp a
      y <- runExp b
      return $ if toBool x then x else y
    And a b -> do
      x <- runExp a
      y <- runExp b
      return $ if toBool x then y else x
    Not a -> (Bool . not . toBool) <$> runExp a
    Add a' b' -> do
      a <- runExp a'
      b <- runExp b'
      case (a, b) of
        (Int x, Int y) -> return $ Int $ x + y
        (Str x, Str y) -> return $ Str $ x <> y
        _ -> pyErr $ UnsupportedOperandTypes
    Sub a b -> evalIntOp a b (-)
    Mul a b -> evalIntOp a b (*)
    Div a b -> evalIntOp a b div
    Mod a b -> evalIntOp a b mod
    Neg a -> do
      x <- runExp a
      case x of
        (Int y) -> return $ Int $ negate y
        _ -> pyErr $ UnsupportedOperandTypes
    Call name args -> do
      exprs <- mapM runExp args
      fun <- access name
      case fun of
        Fun f -> f exprs
        _ -> pyErr $ UnsupportedOperandTypes


-- Execute statement block in the Runner monad
runStatements :: Statements -> Runner (Maybe Val)
runStatements [] = return Nothing
runStatements (s:xs) = do
  case s of
    Exp exp -> runExp exp >> runStatements xs
    If exp a b -> do
      v <- toBool <$> runExp exp
      if v then runStatements a `continue` runStatements xs
           else runStatements b `continue` runStatements xs
    Return exp -> Just <$> runExp exp
    While exp a -> do
      v <- toBool <$> runExp exp
      if v then runStatements a `continue` runStatements (s:xs)
           else runStatements xs
    Def name argNames a -> do
      declare name $ Fun $ \args -> do
        when (length args /= length argNames) $ do
           pyErr $ WrongNumberOfArguments name
        parentScope <- get
        for_ (zip argNames args) $ \(name, value) -> do
          declare name value
        res <- runStatements a
        put parentScope
        case res of
          Just x -> return x
          Nothing -> return None
      runStatements xs
    Assn name exp -> runExp exp >>= declare name >> runStatements xs

-- Execute Python program
run :: Statements -> IO ()
run statements = void $ runStateT (runStatements statements) globals
