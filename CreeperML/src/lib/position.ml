(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Position = struct
  type loc = { start_p : Lexing.position; end_p : Lexing.position }
  type 'a position = { value : 'a; pos : loc }

  let value { value = v; pos = _ } = v
  let position { value = _; pos = p } = p
  let start_p { start_p = s; end_p = _ } = s
  let end_p { start_p = _; end_p = e } = e
  let start_position p = position p |> start_p
  let end_position p = position p |> end_p
  let with_position start_p end_p v = { value = v; pos = { start_p; end_p } }
end
