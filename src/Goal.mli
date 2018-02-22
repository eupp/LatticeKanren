module Make (T : Lattice.T) :
  sig
    type t

    (* val top : t
    val bot : t

    val is_top : t -> bool
    val is_bot : t -> bool *)

    val delay : (unit -> t) -> t

    val lift : T.t -> t

    val (&>>) : t -> t -> t
    val (|>>) : t -> t -> t

    val (<&>) : t -> t -> t
    val (<|>) : t -> t -> t

    val run : ?n:int -> t -> T.t list
  end
