let rec pow2 n =
  if n <= 0 then 1 else
  pow2 (n - 1) + pow2 (n - 1)
in
print_int (pow2 30)
