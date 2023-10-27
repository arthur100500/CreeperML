module Counter = struct
  let cnt = ref 0

  let cnt_next () =
    cnt := !cnt + 1;
    !cnt
end
