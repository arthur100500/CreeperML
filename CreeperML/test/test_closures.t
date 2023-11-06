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
  letc 11 (9) [7, 8] = 
    if ((v1 v8) 1) then v9 else ((v7 ((v2 v8) 1)) ((v3 v8) v9))
  
  letc 12 (8) [7] = 
    clsr[11][7, 8]
  
  letc 13 (6) [] = 
    let 7 =   
      clsr[12][7]
    ((v7 v6) 1)
  
  let 10 = 
    clsr[13][]

+-------------------------+
|  Simple function with   |
|  no variables from env  |
+-------------------------+
  $ ./test_closures.exe <<- EOF
  > let f x =
  >   let g y = y + 1 in
  >   g 
  letc 10 (7) [] = 
    ((v4 v7) 1)
  
  letc 11 (6) [] = 
    let 8 =   
      clsr[10][]
    v8
  
  let 9 = 
    clsr[11][]

+----------------------------------------+
|  Simple function capturing environment |
|  x is bound to 6 and gived to g (10)   |
+----------------------------------------+
  $ ./test_closures.exe <<- EOF
  > let f x =
  >   let g y = y + x + 1 in
  >   g 
  letc 10 (7) [6] = 
    ((v4 ((v4 v7) v6)) 1)
  
  letc 11 (6) [] = 
    let 8 =   
      clsr[10][6]
    v8
  
  let 9 = 
    clsr[11][]

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
  letc 19 (11) [6, 7, 8, 9, 10] = 
    ((v4 ((v4 ((v4 ((v4 ((v4 v6) v7)) v8)) v9)) v10)) v11)
  
  letc 20 (10) [6, 7, 8, 9] = 
    let 12 =   
      clsr[19][6, 7, 8, 9, 10]
    v12
  
  letc 21 (9) [6, 7, 8] = 
    let 13 =   
      clsr[20][6, 7, 8, 9]
    v13
  
  letc 22 (8) [6, 7] = 
    let 14 =   
      clsr[21][6, 7, 8]
    v14
  
  letc 23 (7) [6] = 
    let 15 =   
      clsr[22][6, 7]
    v15
  
  letc 24 (6) [] = 
    let 16 =   
      clsr[23][6]
    v16
  
  let 17 = 
    clsr[24][]
  
  let 18 = 
    ((((((v17 1) 2) 3) 4) 5) 6)
