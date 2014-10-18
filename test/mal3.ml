let rec fib n =
  if n <= 1 then n else
  fib (n - 1) + fib (n - 2) (n - 3) in
print_int (fib 30)

