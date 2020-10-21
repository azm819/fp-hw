def collatz(n):
  if n <= 1:
    return 0
  if n % 2 == 0:
    n = n / 2
  else:
    n = 3 * n + 1
  return 1 + collatz(n)

print(collatz(int(input())))
