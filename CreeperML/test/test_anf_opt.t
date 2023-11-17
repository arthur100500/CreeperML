+-------------------------+
|  ANF Optimization       |
|   1. Reduce move count  |
|   2. ???                |
+-------------------------+

+------------------+
|  Factorial test  |
+------------------+
  $ ./test_anf_opt.exe <<- EOF
  > let fac n =
  > let rec helper n acc =
  > if n <= 1 then 
  > acc
  > else
  > helper (n - 1) (n * acc)
  > in
  > helper n 1
  > EOF
  
  letc 10 (6) [] = 
    let 7 =   
      clsr[7][]
    ((v7 v6) 1)
  
  let 10 = 
    clsr[10][]

+--------------------------------+
|  A lot of moves test           |
|  All these moves are reduced   |
|  And only few values are left  |
|                                |
|  Reduced to something like:    |
|    v6 = 3                      |
|    v11 = 1                     |
|    v18 = 3 + 1 (interm. v17)   |
+--------------------------------+
  $ ./test_anf_opt.exe <<- EOF
  > let a = 3
  > let b = a
  > let c = b
  > let d = c
  > let e = d
  > 
  > let a = 1
  > let f = a
  > let g = f
  > let h = g
  > let i = h
  > 
  > let res = d + i
  > EOF

+----------+
|  Tuples  |
+----------+
  $ ./test_anf_opt.exe <<- EOF
  > let (a, b, c) = (10 + 11, 12 + 13, 14 + 15)
  > let x = (a, (b, c))
  > let (c, (d, e)) = x
  > let res = c + d + e
  > EOF

+-----------------------------+
|  Simple test with closures  |
+-----------------------------+
  $ ./test_anf_opt.exe <<- EOF
  > let f x =
  >   fun y ->
  >     let g z = 
  >       z + y + x
  >     in g
  > 
  > let ires = f 10 11
  > let res = ires 22
  > EOF


+--------------------------------+
|  Big goody test                |
|  According to my calculations  |
|  It reduced 28 lines of code   |
+--------------------------------+
  $ ./test_anf_opt.exe <<- EOF
  > let a = 10 + 11 * 32
  > let b = 8 + a * 4
  > let c = b
  > let d = c
  > let e = 3 + c * 2 * b * b
  > let f x = 33 + 22 * x
  > let g a b c =
  >   let h d e f =
  >      a + (b * c) - d * e + 2 * f
  >   in h
  > let p = c + d + g 10 11 12 13 14 15
  > let s = g 55 66 77 88
  > let res = a + b - c + d * (s e (f 19)) + p
  > EOF

