module Position : sig
  (* position of start and end symbol in code *)
  type loc

  (* something with it position in code *)
  type 'a position

  (* get value *)
  val value : 'a position -> 'a

  (* get position *)
  val position : 'a position -> loc

  (* get position of start symbol *)
  val start_position : 'a position -> Lexing.position

  (* get position of end symbol *)
  val end_position : 'a position -> Lexing.position

  (* add position to value *)
  val with_position : Lexing.position -> Lexing.position -> 'a -> 'a position
end
