(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Counter : sig
  val cnt_next : unit -> int
  val snd_next : unit -> int
  val third_next : unit -> int
end
