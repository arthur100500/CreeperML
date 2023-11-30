(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Std = struct
  module Names = struct
    open Counter.Counter
    module NameMap = Map.Make (String)

    let add name = NameMap.add name (cnt_next ())
    let ( ||> ) m name = m |> add name

    let names =
      List.fold_left ( ||> ) NameMap.empty
        [
          "-";
          "+";
          "*";
          "/";
          "<=";
          "<";
          "==";
          ">";
          ">=";
          "-.";
          "+.";
          "*.";
          "/.";
          "<=.";
          "<.";
          "==.";
          ">.";
          ">=.";
          "print_int";
          "print_string";
        ]

    let find i =
      NameMap.bindings names
      |> List.filter (fun (_, ix) -> Int.equal i ix)
      |> List.hd
  end

  module Inferencer = struct
    open Counter.Counter
    open Type_ast.TypeAst
    open Type_ast.InferTypeUtils

    let find = snd_next () |> Names.find |> fst

    (* *)
    let int_const = t_ground t_int |> with_lvls 0 0
    let unit_const = t_ground t_unit |> with_lvls 0 0
    let bool_const = t_ground t_bool |> with_lvls 0 0
    let float_const = t_ground t_float |> with_lvls 0 0
    let string_const = t_ground t_string |> with_lvls 0 0
    let arr l r = t_arrow l r |> with_lvls 0 0
    let triple fst snd rez = arr snd rez |> t_arrow fst |> with_lvls 0 0

    (* int operations *)
    let sub = (find, triple int_const int_const int_const)
    let add = (find, triple int_const int_const int_const)
    let mul = (find, triple int_const int_const int_const)
    let div = (find, triple int_const int_const int_const)

    (* int bool *)
    let le = (find, triple int_const int_const bool_const)
    let less = (find, triple int_const int_const bool_const)
    let eq = (find, triple int_const int_const bool_const)
    let gr = (find, triple int_const int_const bool_const)
    let ge = (find, triple int_const int_const bool_const)

    (* float operations *)
    let fsub = (find, triple float_const float_const float_const)
    let fadd = (find, triple float_const float_const float_const)
    let fmul = (find, triple float_const float_const float_const)
    let fdiv = (find, triple float_const float_const float_const)

    (* float bool*)
    let fle = (find, triple float_const float_const bool_const)
    let fless = (find, triple float_const float_const bool_const)
    let feq = (find, triple float_const float_const bool_const)
    let fgr = (find, triple float_const float_const bool_const)
    let fge = (find, triple float_const float_const bool_const)

    (* prints *)
    let print_int = (find, arr int_const unit_const)
    let print_string = (find, arr string_const unit_const)

    let env =
      [
        sub;
        add;
        mul;
        div;
        le;
        less;
        eq;
        gr;
        ge;
        fsub;
        fadd;
        fmul;
        fdiv;
        fle;
        fless;
        feq;
        fgr;
        fge;
        print_int;
        print_string;
      ]
  end

  module Operators = struct
    open Counter.Counter
    open Type_ast.TypeAst

    let typed t a : ('a, ty) typed = { value = a; typ = t }

    (* *)
    let int_const = TyGround TInt
    let unit_const = TyGround TUnit
    let bool_const = TyGround TBool
    let float_const = TyGround TFloat
    let string_const = TyGround TString
    let arr l r = TyArrow (l, r)
    let triple fst snd rez = TyArrow (TyArrow (fst, snd), rez)

    (* int operations *)
    let sub = typed (triple int_const int_const int_const) (third_next ())
    let add = typed (triple int_const int_const int_const) (third_next ())
    let mul = typed (triple int_const int_const int_const) (third_next ())
    let div = typed (triple int_const int_const int_const) (third_next ())

    (* int bool *)
    let le = typed (triple int_const int_const bool_const) (third_next ())
    let less = typed (triple int_const int_const bool_const) (third_next ())
    let eq = typed (triple int_const int_const bool_const) (third_next ())
    let gr = typed (triple int_const int_const bool_const) (third_next ())
    let ge = typed (triple int_const int_const bool_const) (third_next ())

    (* float operations *)
    let fsub =
      typed (triple float_const float_const float_const) (third_next ())

    let fadd =
      typed (triple float_const float_const float_const) (third_next ())

    let fmul =
      typed (triple float_const float_const float_const) (third_next ())

    let fdiv =
      typed (triple float_const float_const float_const) (third_next ())

    (* float bool*)
    let fle = typed (triple float_const float_const bool_const) (third_next ())

    let fless =
      typed (triple float_const float_const bool_const) (third_next ())

    let feq = typed (triple float_const float_const bool_const) (third_next ())
    let fgr = typed (triple float_const float_const bool_const) (third_next ())
    let fge = typed (triple float_const float_const bool_const) (third_next ())

    (* prints *)
    let print_int = typed (arr int_const unit_const) (third_next ())
    let print_string = typed (arr string_const unit_const) (third_next ())

    let operators =
      [
        sub;
        add;
        mul;
        div;
        le;
        less;
        eq;
        gr;
        ge;
        fsub;
        fadd;
        fmul;
        fdiv;
        fle;
        fless;
        feq;
        fgr;
        fge;
        print_int;
        print_string;
      ]
  end

  let typeenv = Inferencer.env
  let names = Names.names
  let operators = Operators.operators
end
