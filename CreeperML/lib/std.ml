(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Std = struct
  module Names = struct
    open Counter
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
  end

  module Inferencer = struct
    open Type_ast.InferTypeUtils

    let top_lvl = with_lvls 0 0

    (* *)
    let int_const = t_ground t_int |> top_lvl
    let unit_const = t_ground t_unit |> top_lvl
    let bool_const = t_ground t_bool |> top_lvl
    let float_const = t_ground t_float |> top_lvl
    let string_const = t_ground t_string |> top_lvl
    let arr l r = t_arrow l r |> top_lvl
    let triple fst snd rez = arr snd rez |> t_arrow fst |> top_lvl

    (* int operations *)
    let sub = ("-", triple int_const int_const int_const)
    let add = ("+", triple int_const int_const int_const)
    let mul = ("*", triple int_const int_const int_const)
    let div = ("/", triple int_const int_const int_const)

    (* int bool *)
    let le = ("<=", triple int_const int_const bool_const)
    let less = ("<", triple int_const int_const bool_const)
    let eq = ("==", triple int_const int_const bool_const)
    let gr = (">", triple int_const int_const bool_const)
    let ge = (">=", triple int_const int_const bool_const)

    (* float operations *)
    let fsub = ("-.", triple float_const float_const float_const)
    let fadd = ("+.", triple float_const float_const float_const)
    let fmul = ("*.", triple float_const float_const float_const)
    let fdiv = ("/.", triple float_const float_const float_const)

    (* float bool*)
    let fle = ("<=.", triple float_const float_const bool_const)
    let fless = ("<.", triple float_const float_const bool_const)
    let feq = ("==.", triple float_const float_const bool_const)
    let fgr = (">=.", triple float_const float_const bool_const)
    let fge = (">.", triple float_const float_const bool_const)

    (* prints *)
    let print_int = ("print_int", arr int_const unit_const)
    let print_string = ("print_string", arr string_const unit_const)

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
    let sub = typed (triple int_const int_const int_const) 1
    let add = typed (triple int_const int_const int_const) 2
    let mul = typed (triple int_const int_const int_const) 3
    let div = typed (triple int_const int_const int_const) 4

    (* int bool *)
    let le = typed (triple int_const int_const bool_const) 5
    let less = typed (triple int_const int_const bool_const) 6
    let eq = typed (triple int_const int_const bool_const) 7
    let gr = typed (triple int_const int_const bool_const) 8
    let ge = typed (triple int_const int_const bool_const) 9

    (* float operations *)
    let fsub = typed (triple float_const float_const float_const) 10
    let fadd = typed (triple float_const float_const float_const) 11
    let fmul = typed (triple float_const float_const float_const) 12
    let fdiv = typed (triple float_const float_const float_const) 13

    (* float bool*)
    let fle = typed (triple float_const float_const bool_const) 14
    let fless = typed (triple float_const float_const bool_const) 15
    let feq = typed (triple float_const float_const bool_const) 16
    let fgr = typed (triple float_const float_const bool_const) 17
    let fge = typed (triple float_const float_const bool_const) 18

    (* prints *)
    let print_int = typed (arr int_const unit_const) 19
    let print_string = typed (arr string_const unit_const) 20

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
