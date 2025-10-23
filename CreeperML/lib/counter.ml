(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Counter = struct
  let cnt = ref 0

  let cnt_next () =
    cnt := !cnt + 1;
    !cnt

  let sndcnt = ref 0

  let snd_next () =
    sndcnt := !sndcnt + 1;
    !sndcnt

  let third = ref 0

  let third_next () =
    third := !third + 1;
    !third
end
