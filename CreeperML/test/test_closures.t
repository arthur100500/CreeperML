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
  letc 7 (8) (9) = 
    if ((v1 v8) 1) then v9 else ((v7 ((v2 v8) 1)) ((v3 v8) v9))
  
  letc 10 (6) = 
    let 7 =   
      v7
    ((v7 v6) 1)
  
  let 10 = 
    v10

+-------------------------+
|  Simple function with   |
|  no variables from env  |
+-------------------------+
  $ ./test_closures.exe <<- EOF
  > let f x =
  >   let g y = y + 1 in
  >   g 
  > EOF
  letc 8 (7) = 
    ((v4 v7) 1)
  
  letc 9 (6) = 
    let 8 =   
      v8
    v8
  
  let 9 = 
    v9

+----------------------------------------+
|  Simple function capturing environment |
|  x is bound to 6 and gived to g (10)   |
+----------------------------------------+
  $ ./test_closures.exe <<- EOF
  > let f x =
  >   let g y = y + x + 1 in
  >   g 
  > EOF
  letc 8 (6) (7) = 
    ((v4 ((v4 v7) v6)) 1)
  
  letc 9 (6) = 
    let 8 =   
      (v8 v6)
    v8
  
  let 9 = 
    v9


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
  > EOF
  letc 12 (6) (7) (8) (9) (10) (11) = 
    ((v4 ((v4 ((v4 ((v4 ((v4 v6) v7)) v8)) v9)) v10)) v11)
  
  letc 13 (6) (7) (8) (9) (10) = 
    let 12 =   
      (v12 v6 v7 v8 v9 v10)
    v12
  
  letc 14 (6) (7) (8) (9) = 
    let 13 =   
      (v13 v6 v7 v8 v9)
    v13
  
  letc 15 (6) (7) (8) = 
    let 14 =   
      (v14 v6 v7 v8)
    v14
  
  letc 16 (6) (7) = 
    let 15 =   
      (v15 v6 v7)
    v15
  
  letc 17 (6) = 
    let 16 =   
      (v16 v6)
    v16
  
  let 17 = 
    v17
  
  let 18 = 
    ((((((v17 1) 2) 3) 4) 5) 6)
