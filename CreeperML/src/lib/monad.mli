module Result : sig
  type 'a t = ('a, string) Result.t

  val return : 'a -> 'a t
  val error : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
end
