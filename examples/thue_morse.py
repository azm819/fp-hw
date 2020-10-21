def thue_morse(n):
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

print(thue_morse(int(input())))
