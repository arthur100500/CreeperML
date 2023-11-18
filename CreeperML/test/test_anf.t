+------------------+
|  Factorial test  |
+------------------+
  $ ./test_anf.exe <<- EOF
  > let fac n =
  > let rec helper n acc =
  > if n <= 1 then 
  > acc
  > else
  > helper (n - 1) (n * acc)
  > in
  > helper n 1
  > EOF


+------------------------+
|  Simple expr test      |
|  a + b + c             |
|  Converts to:          |
|    v7 = (+) 1          |
|    v8 = 1 + 2          |
|    v9 = (+) (1 + 2)    |
|    v10 = (1 + 2) + 3   |
|    res(v6) = v10       |
+------------------------+
  $ ./test_anf.exe <<- EOF
  > let res = 1 + 2 + 3
  > EOF


+-----------------------------------+
|  Test with functions              |
|  Converts to:                     |
|  g(11) y(15) [x(6)] =             |
|    v7 = y                         |
|    v13 = (+) x                    |
|    v14 = x + y                    |
|    return x + y                   |
|                                   |
|  f(12) x(17) [no env] =           |
|    v6 = x                         |
|    v16 = alloc clsr. g with [x=x] |
|    v8 = allocated g               |
|    return allocated g             |
|                                   |
|  And then result is calculated    |
|    ...                            |
+-----------------------------------+
  $ ./test_anf.exe <<- EOF
  > let f x =
  >   let g y =
  >     x + y
  >   in g
  > let res = f 10 11
  > EOF


+----------------------------------+
|  If-then-else test               |
|  To conserve the lazy nature     |
|    of if-then-else, ANF blocks   |
|    are left inside the branches  |
|    and condition                 |
|                                  |
|  Converts to:                    |
|    v15 = if                      |
|      v7 = (+) 3                  |
|      v8 = 3 + 5                  |
|      v9 = (<=) (3 + 5)           |
|      v10 = 3 + 5 <= 22           |
|      res is 3 + 5 <= 22          |
|    then                          |
|      v11 = (-) 2                 |
|      v12 = 2 - 3                 |
|      res is 2 - 3                |
|    else                          |
|      v13 = (+) 4                 |
|      v14 = 4 + 5                 |
|      res is 4 + 5                |
|    res is ite(15)                |
|                                  |
+----------------------------------+
  $ ./test_anf.exe <<- EOF
  > let r = if 3 + 5 <= 22 then 2 - 3 else 4 + 5
  > EOF

+-----------------------+
|  More if-then-else    |
|  more for testing pp  |
+-----------------------+
  $ ./test_anf.exe <<- EOF
  > let r = if (if 2 - 10 <= 2 then false else true) then 2 - 3 else (if true then 77 - 33 + 23 else (25 + 11 * 3))
  > EOF

+--------------------------+
|  Tuples                  |
|  Decomposing a tuple is  |
|  done like an index      |
+--------------------------+
  $ ./test_anf_opt.exe <<- EOF
  > let (a, b, c) = (10 + 11, 12 + 13, 14 + 15)
  > let x = (a, (b, c))
  > let (c, (d, e)) = x
  > let res = c + d + e
  > EOF

+------------------+
|  Big goody test  |
+------------------+
  $ ./test_anf.exe <<- EOF
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
