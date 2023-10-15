  $ ./test_parser.exe <<- EOF
  > let rec fac n =
  > if n <= 0 then
  > 1
  > else
  > fac (n - 1)
  > EOF
  [{ value =
     { rec_f = Rec;
       l_v =
       { value = (LvValue "fac"); pos = {start_p: [: 1 0 8]; end_p: [: 1 0 11]}
         };
       args =
       [{ value = (LvValue "n"); pos = {start_p: [: 1 0 12]; end_p: [: 1 0 13]}
          }
         ];
       body =
       { value =
         { lets = [];
           expr =
           { value =
             EFun {
               lvalue =
               { value = (LvValue "n");
                 pos = {start_p: [: 1 0 12]; end_p: [: 1 0 13]} };
               body =
               { value =
                 { lets = [];
                   expr =
                   { value =
                     EIfElse {
                       cond =
                       { value =
                         (EApply (
                            { value =
                              (EApply (
                                 { value = (EValue "<=");
                                   pos =
                                   {start_p: [: 1 0 21]; end_p: [: 1 0 23]} },
                                 { value = (EValue "n");
                                   pos =
                                   {start_p: [: 1 0 19]; end_p: [: 1 0 20]} }
                                 ));
                              pos = {start_p: [: 1 0 19]; end_p: [: 1 0 25]} },
                            { value =
                              (ELiteral
                                 { value = (LInt 0);
                                   pos =
                                   {start_p: [: 1 0 24]; end_p: [: 1 0 25]} });
                              pos = {start_p: [: 1 0 24]; end_p: [: 1 0 25]} }
                            ));
                         pos = {start_p: [: 1 0 19]; end_p: [: 1 0 25]} };
                       t_body =
                       { value =
                         (ELiteral
                            { value = (LInt 1);
                              pos = {start_p: [: 1 0 31]; end_p: [: 1 0 32]} });
                         pos = {start_p: [: 1 0 31]; end_p: [: 1 0 32]} };
                       f_body =
                       { value =
                         (EApply (
                            { value = (EValue "fac");
                              pos = {start_p: [: 1 0 38]; end_p: [: 1 0 41]} },
                            { value =
                              (EApply (
                                 { value =
                                   (EApply (
                                      { value = (EValue "-");
                                        pos =
                                        {start_p: [: 1 0 45]; end_p: [: 1 0 46]}
                                        },
                                      { value = (EValue "n");
                                        pos =
                                        {start_p: [: 1 0 43]; end_p: [: 1 0 44]}
                                        }
                                      ));
                                   pos =
                                   {start_p: [: 1 0 43]; end_p: [: 1 0 48]} },
                                 { value =
                                   (ELiteral
                                      { value = (LInt 1);
                                        pos =
                                        {start_p: [: 1 0 47]; end_p: [: 1 0 48]}
                                        });
                                   pos =
                                   {start_p: [: 1 0 47]; end_p: [: 1 0 48]} }
                                 ));
                              pos = {start_p: [: 1 0 43]; end_p: [: 1 0 48]} }
                            ));
                         pos = {start_p: [: 1 0 38]; end_p: [: 1 0 49]} }};
                     pos = {start_p: [: 1 0 16]; end_p: [: 1 0 49]} }
                   };
                 pos = {start_p: [: 1 0 15]; end_p: [: 1 0 49]} }};
             pos = {start_p: [: 1 0 0]; end_p: [: 1 0 49]} }
           };
         pos = {start_p: [: 1 0 0]; end_p: [: 1 0 49]} }
       };
     pos = {start_p: [: 1 0 0]; end_p: [: 1 0 49]} }
    ]
