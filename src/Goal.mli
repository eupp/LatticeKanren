module Make (T : Lattice.T) :
  sig
    type t

    val delay : (unit -> t) -> t

    val lift : T.t -> t

    (* unfair asymmetric conjunction and disjunction *)
    val (&>>) : t -> t -> t
    val (|>>) : t -> t -> t

    (* fair asymmetric conjunction and disjunction *)
    val (<&>) : t -> t -> t
    val (<|>) : t -> t -> t

    val run : ?n:int -> t -> T.t list
  end
