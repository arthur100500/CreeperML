  $ ./test_infer.exe <<- EOF
  > let rec fac n =
  > if n <= 0 then
  > 1
  > else
  > fac (n - 1)
  > EOF
  (TyArrow ((TyGround TInt), (TyGround TInt)))
