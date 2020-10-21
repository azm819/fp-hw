def fun1(y, z):
  x = y % 3 + z * 2
  if x > 5 and y <= 10 or z == 0:
    return x + y + z
  elif z < 3:
    return z * z * z
  return x

def fun2(n):
  if n <= 0:
    return 0
  cnt = 0
  while n != 1:
    if n % 2 == 0:
      n = n / 2
    else:
      n = 3 * n + 1
    cnt = cnt + 1
  return cnt

def fun2r(n):
  if n <= 1:
    return 0
  if n % 2 == 0:
    n = n / 2
  else:
    n = 3 * n + 1
  return 1 + fun2r(n)

def fun3(n):
  if n < 0:
    return ""
  a = "0"
  b = "1"
  while n > 0:
    n = n - 1
    x = a + b
    y = b + a
    a = x
    b = y
  return a

