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
  let (11) (23) [7, 8] = 
    let (14) = v(1) v(8)
    let (15) = v(14) l(1)
    let (22) = if v(15) then
      v(23)
    else
      let (16) = v(2) v(8)
      let (17) = v(16) l(1)
      let (18) = v(7) v(17)
      let (19) = v(3) v(8)
      let (20) = v(19) v(23)
      let (21) = v(18) v(20)
      v(21)
    v(22)
  
  let (12) (25) [7] = 
    let (24) = clsr[v(11)][v(7), v(25)]
    v(24)
  
  let (13) (29) [] = 
    let (26) = clsr[v(12)][v(7)]
    let (27) = v(26) v(29)
    let (28) = v(27) l(1)
    v(28)
  
  let (30) = clsr[v(13)][]

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
  let (6) = l(3)
  
  let (11) = l(1)
  
  let (17) = v(4) v(6)
  
  let (18) = v(17) v(11)

+----------+
|  Tuples  |
+----------+
  $ ./test_anf_opt.exe <<- EOF
  > let (a, b, c) = (10 + 11, 12 + 13, 14 + 15)
  > let x = (a, (b, c))
  > let (c, (d, e)) = x
  > let res = c + d + e
  > EOF
  let (14) = v(4) l(10)
  
  let (15) = v(14) l(11)
  
  let (16) = v(4) l(12)
  
  let (17) = v(16) l(13)
  
  let (18) = v(4) l(14)
  
  let (19) = v(18) l(15)
  
  let (20) = (v(15), v(17), v(19))
  
  let (21) = v(20)[0]
  
  let (22) = v(20)[1]
  
  let (23) = v(20)[2]
  
  let (24) = (v(22), v(23))
  
  let (25) = (v(21), v(24))
  
  let (26) = v(25)[0]
  
  let (27) = v(25)[1]
  
  let (28) = v(27)[0]
  
  let (29) = v(27)[1]
  
  let (30) = v(4) v(26)
  
  let (31) = v(30) v(28)
  
  let (32) = v(4) v(31)
  
  let (33) = v(32) v(29)

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
  let (13) (20) [6, 7] = 
    let (16) = v(4) v(20)
    let (17) = v(16) v(7)
    let (18) = v(4) v(17)
    let (19) = v(18) v(6)
    v(19)
  
  let (14) (22) [6] = 
    let (21) = clsr[v(13)][v(6), v(22)]
    v(21)
  
  let (15) (24) [] = 
    let (23) = clsr[v(14)][v(24)]
    v(23)
  
  let (25) = clsr[v(15)][]
  
  let (26) = v(25) l(10)
  
  let (27) = v(26) l(11)
  
  let (28) = v(27) l(22)


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
  let (31) = v(4) l(10)
  
  let (32) = v(3) l(11)
  
  let (33) = v(32) l(32)
  
  let (34) = v(31) v(33)
  
  let (35) = v(4) l(8)
  
  let (36) = v(3) v(34)
  
  let (37) = v(36) l(4)
  
  let (38) = v(35) v(37)
  
  let (39) = v(4) l(3)
  
  let (40) = v(3) v(38)
  
  let (41) = v(40) l(2)
  
  let (42) = v(3) v(41)
  
  let (43) = v(42) v(38)
  
  let (44) = v(3) v(43)
  
  let (45) = v(44) v(38)
  
  let (46) = v(39) v(45)
  
  let (24) (51) [] = 
    let (47) = v(4) l(33)
    let (48) = v(3) l(22)
    let (49) = v(48) v(51)
    let (50) = v(47) v(49)
    v(50)
  
  let (52) = clsr[v(24)][]
  
  let (25) (65) [13, 14, 15, 16, 17] = 
    let (53) = v(4) v(13)
    let (54) = v(3) v(14)
    let (55) = v(54) v(15)
    let (56) = v(53) v(55)
    let (57) = v(2) v(56)
    let (58) = v(3) v(16)
    let (59) = v(58) v(17)
    let (60) = v(57) v(59)
    let (61) = v(4) v(60)
    let (62) = v(3) l(2)
    let (63) = v(62) v(65)
    let (64) = v(61) v(63)
    v(64)
  
  let (26) (67) [13, 14, 15, 16] = 
    let (66) = clsr[v(25)][v(13), v(14), v(15), v(16), v(67)]
    v(66)
  
  let (27) (69) [13, 14, 15] = 
    let (68) = clsr[v(26)][v(13), v(14), v(15), v(69)]
    v(68)
  
  let (28) (71) [13, 14] = 
    let (70) = clsr[v(27)][v(13), v(14), v(71)]
    v(70)
  
  let (29) (73) [13] = 
    let (72) = clsr[v(28)][v(13), v(73)]
    v(72)
  
  let (30) (75) [] = 
    let (74) = clsr[v(29)][v(75)]
    v(74)
  
  let (76) = clsr[v(30)][]
  
  let (77) = v(4) v(38)
  
  let (78) = v(77) v(38)
  
  let (79) = v(4) v(78)
  
  let (80) = v(76) l(10)
  
  let (81) = v(80) l(11)
  
  let (82) = v(81) l(12)
  
  let (83) = v(82) l(13)
  
  let (84) = v(83) l(14)
  
  let (85) = v(84) l(15)
  
  let (86) = v(79) v(85)
  
  let (87) = v(76) l(55)
  
  let (88) = v(87) l(66)
  
  let (89) = v(88) l(77)
  
  let (90) = v(89) l(88)
  
  let (91) = v(4) v(34)
  
  let (92) = v(91) v(38)
  
  let (93) = v(2) v(92)
  
  let (94) = v(93) v(38)
  
  let (95) = v(4) v(94)
  
  let (96) = v(3) v(38)
  
  let (97) = v(90) v(46)
  
  let (98) = v(52) l(19)
  
  let (99) = v(97) v(98)
  
  let (100) = v(96) v(99)
  
  let (101) = v(95) v(100)
  
  let (102) = v(4) v(101)
  
  let (103) = v(102) v(86)
