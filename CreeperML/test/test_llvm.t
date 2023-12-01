  $ cat > input.ml <<- EOF
  > let fac n =
  >   let rec helper n acc =
  >     if n <= 1 then 
  >       acc
  >     else
  >       helper (n - 1) (n * acc)
  >     in
  >   helper n 1
  > let () = print_int (fac 5)
  > EOF
  $ ocaml input.ml
  120
  $ cat input.ml | ./test_llvm.exe
  $ opt -f -S test -o test-opt.ll -Oz 
  $ llc --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  120

  $ cat > input.ml <<- EOF
  > let (a, b, c, d) = (1, 2, 3, 4)
  > let sum = a + b + c + d
  > let () = print_int sum
  > EOF
  $ ocaml input.ml
  10
  $ cat input.ml | ./test_llvm.exe
  $ opt -f -S test -o test-opt.ll -Oz 
  $ llc --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  10

  $ cat > input.ml <<- EOF
  > let id a = a
  > let () = print_string (id "Hello, world")
  > EOF
  $ ocaml input.ml
  Hello, world
  $ cat input.ml | ./test_llvm.exe
  $ opt -f -S test -o test-opt.ll -Oz 
  $ llc --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  Hello, world

  $ cat > input.ml <<- EOF
  > let foo a b = a + b
  > let baz a = foo 1 a
  > let () = print_int (baz 9)
  > EOF
  $ ocaml input.ml
  10
  $ cat input.ml | ./test_llvm.exe
  $ opt -f -S test -o test-opt.ll -Oz 
  $ llc --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  10
