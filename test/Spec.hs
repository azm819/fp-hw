import Test.HUnit hiding (assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO.Silently
import Control.Monad (void)

import AST
import Parser
import qualified Runner as R

progPy = "def fun1(y, z):\n\
\  x = y % 3 + z * 2\n\
\  if x > 5 and y <= 10 or z == 0:\n\
\    return x + y + z\n\
\  elif z < 3:\n\
\    return z * z * z\n\
\  return x\n\
\\n\
\def fun2(n):\n\
\  if n <= 0:\n\
\    return 0\n\
\  cnt = 0\n\
\  while n != 1:\n\
\    if n % 2 == 0:\n\
\      n = n / 2\n\
\    else:\n\
\      n = 3 * n + 1\n\
\    cnt = cnt + 1\n\
\  return cnt\n\
\\n\
\def fun2r(n):\n\
\  if n <= 1:\n\
\    return 0\n\
\  if n % 2 == 0:\n\
\    n = n / 2\n\
\  else:\n\
\    n = 3 * n + 1\n\
\  return 1 + fun2r(n)\n\
\\n\
\def fun3(n):\n\
\  if n < 0:\n\
\    return \"\"\n\
\  a = \"0\"\n\
\  b = \"1\"\n\
\  while n > 0:\n\
\    n = n - 1\n\
\    x = a + b\n\
\    y = b + a\n\
\    a = x\n\
\    b = y\n\
\  return a\n"

progEdsl = "edsl $ do\n\
\  def \"fun1\" [\"y\",\"z\"] $ do\n\
\    \"x\" $=$ n \"y\" $%$ i 3 $+$ n \"z\" $*$ i 2\n\
\    if' (n \"x\" $>$ i 5 `and'` n \"y\" $<=$ i 10 `or'` n \"z\" $==$ i 0) $ do\n\
\      return' $ (n \"x\" $+$ n \"y\") $+$ n \"z\"\n\
\    else' $ do\n\
\      if' (n \"z\" $<$ i 3) $ do\n\
\        return' $ (n \"z\" $*$ n \"z\") $*$ n \"z\"\n\
\    \n\
\    return' $ n \"x\"\n\
\  \n\
\  def \"fun2\" [\"n\"] $ do\n\
\    if' (n \"n\" $<=$ i 0) $ do\n\
\      return' $ i 0\n\
\    \n\
\    \"cnt\" $=$ i 0\n\
\    while (n \"n\" $!=$ i 1) $ do\n\
\      if' (n \"n\" $%$ i 2 $==$ i 0) $ do\n\
\        \"n\" $=$ n \"n\" $/$ i 2\n\
\      else' $ do\n\
\        \"n\" $=$ i 3 $*$ n \"n\" $+$ i 1\n\
\      \n\
\      \"cnt\" $=$ n \"cnt\" $+$ i 1\n\
\    \n\
\    return' $ n \"cnt\"\n\
\  \n\
\  def \"fun2r\" [\"n\"] $ do\n\
\    if' (n \"n\" $<=$ i 1) $ do\n\
\      return' $ i 0\n\
\    \n\
\    if' (n \"n\" $%$ i 2 $==$ i 0) $ do\n\
\      \"n\" $=$ n \"n\" $/$ i 2\n\
\    else' $ do\n\
\      \"n\" $=$ i 3 $*$ n \"n\" $+$ i 1\n\
\    \n\
\    return' $ i 1 $+$ \"fun2r\" $$ [n \"n\"]\n\
\  \n\
\  def \"fun3\" [\"n\"] $ do\n\
\    if' (n \"n\" $<$ i 0) $ do\n\
\      return' $ s \"\"\n\
\    \n\
\    \"a\" $=$ s \"0\"\n\
\    \"b\" $=$ s \"1\"\n\
\    while (n \"n\" $>$ i 0) $ do\n\
\      \"n\" $=$ n \"n\" $-$ i 1\n\
\      \"x\" $=$ n \"a\" $+$ n \"b\"\n\
\      \"y\" $=$ n \"b\" $+$ n \"a\"\n\
\      \"a\" $=$ n \"x\"\n\
\      \"b\" $=$ n \"y\"\n\
\    \n\
\    return' $ n \"a\"\n"

prog = do
  def "fun1" ["y","z"] $ do
    "x" $=$ n "y" $%$ i 3 $+$ n "z" $*$ i 2
    if' (n "x" $>$ i 5 `and'` n "y" $<=$ i 10 `or'` n "z" $==$ i 0) $ do
      return' $ (n "x" $+$ n "y") $+$ n "z"
    else' $ do
      if' (n "z" $<$ i 3) $ do
        return' $ (n "z" $*$ n "z") $*$ n "z"

    return' $ n "x"

  def "fun2" ["n"] $ do
    if' (n "n" $<=$ i 0) $ do
      return' $ i 0

    "cnt" $=$ i 0
    while (n "n" $!=$ i 1) $ do
      if' (n "n" $%$ i 2 $==$ i 0) $ do
        "n" $=$ n "n" $/$ i 2
      else' $ do
        "n" $=$ i 3 $*$ n "n" $+$ i 1

      "cnt" $=$ n "cnt" $+$ i 1

    return' $ n "cnt"

  def "fun2r" ["n"] $ do
    if' (n "n" $<=$ i 1) $ do
      return' $ i 0

    if' (n "n" $%$ i 2 $==$ i 0) $ do
      "n" $=$ n "n" $/$ i 2
    else' $ do
      "n" $=$ i 3 $*$ n "n" $+$ i 1

    return' $ i 1 $+$ "fun2r" $$ [n "n"]

  def "fun3" ["n"] $ do
    if' (n "n" $<$ i 0) $ do
      return' $ s ""
    "a" $=$ s "0"
    "b" $=$ s "1"
    while (n "n" $>$ i 0) $ do
      "n" $=$ n "n" $-$ i 1
      "x" $=$ n "a" $+$ n "b"
      "y" $=$ n "b" $+$ n "a"
      "a" $=$ n "x"
      "b" $=$ n "y"

    return' $ n "a"

fun1 :: Int -> Int -> Int
fun1 y z = if x > 5 && y <= 10 || z == 0 then x + y + z
                                         else if z < 3 then z * z * z
                                                       else x
  where x = y `mod` 3 + z * 2

fun2 :: Int -> Int
fun2 n
  | n <= 1 = 0
  | n `mod` 2 == 0 = 1 + fun2 (n `div` 2)
  | otherwise = 1 + fun2 (3 * n + 1)

fun3 :: Int -> String
fun3 n
  | n < 0 = ""
  | n == 0 = "0"
  | otherwise = fun3 (n - 1) <> (map f $ fun3 (n - 1))
  where f '0' = '1'
        f '1' = '0'

prop_fun1 :: Property
prop_fun1 = monadicIO $ do
  y <- pick $ choose (-1000, 1000)
  z <- pick $ choose (-1000, 1000)
  res <- run $ capture_ $ R.run $ edsl (prog >> (e$ "print" $$ ["fun1" $$ [i y, i z]]))
  assert $ res == (show $ fun1 y z) <> "\n"

prop_fun2 :: Property
prop_fun2 = monadicIO $ do
  n <- pick $ choose (0, 1000)
  res <- run $ capture_ $ R.run $ edsl (prog >> (e$ "print" $$ ["fun2" $$ [i n]]))
  assert $ res == (show $ fun2 n) <> "\n"

prop_fun2r :: Property
prop_fun2r = monadicIO $ do
  n <- pick $ choose (0, 1000)
  res <- run $ capture_ $ R.run $ edsl (prog >> (e$ "print" $$ ["fun2r" $$ [i n]]))
  assert $ res == (show $ fun2 n) <> "\n"

prop_fun3 :: Property
prop_fun3 = monadicIO $ do
  n <- pick $ choose (0, 10)
  res <- run $ capture_ $ R.run $ edsl (prog >> (e$ "print" $$ ["fun3" $$ [i n]]))
  assert $ res == fun3 n <> "\n"

testParser = TestCase $ parse "" progPy @?= (Right $ edsl prog)
testPrinter = TestCase $ showProg (edsl prog) @?= progEdsl

main :: IO ()
main = do
  quickCheck prop_fun1
  quickCheck prop_fun2
  quickCheck prop_fun2r
  quickCheck prop_fun3
  void $ runTestTT $ TestList [ TestLabel "parser" testParser, TestLabel "printer" testPrinter ]
