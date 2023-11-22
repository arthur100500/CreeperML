(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val the_module : Llvm.llmodule

val codegen_anf_binding :
  Anf.AnfTypeAst.anf_binding -> Llvm.llvalue Monad.Result.t
