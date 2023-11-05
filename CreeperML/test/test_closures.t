+------------------------------...
|  Format:
|  closure: letc name arg [captured vars list] = ...
|  calling closure: clsr[name][given args]
+------------------------------...

+------------------+
|  Factorial test  |
+------------------+
  $ ./test_closures.exe <<- EOF
  > let fac n =
  > let rec helper n acc =
  > if n <= 1 then 
  > acc
  > else
  > helper (n - 1) (n * acc)
  > in
  > helper n 1
  > EOF


+-------------------------+
|  Simple function with   |
|  no variables from env  |
+-------------------------+
  $ ./test_closures.exe <<- EOF
  > let f x =
  >   let g y = y + 1 in
  >   g 

+----------------------------------------+
|  Simple function capturing environment |
|  x is bound to 6 and gived to g (10)   |
+----------------------------------------+
  $ ./test_closures.exe <<- EOF
  > let f x =
  >   let g y = y + x + 1 in
  >   g 


+--------------------------------------------------+
|  A lot of closures and partial applications      |
|  For each inner function a closure will be done  |
+--------------------------------------------------+
  $ ./test_closures.exe <<- EOF
  > let q a =
  >   let w s =
  >     let e d =
  >       let r f =
  >         let t g =
  >           let y h =
  >             a + s + d + f + g + h in
  >           y in
  >         t in
  >       r in
  >     e in
  >   w
  > let p = q 1 2 3 4 5 6
