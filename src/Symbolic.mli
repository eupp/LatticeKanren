module Make(L : Lattice.T) :
  sig
    type t

    type elt = L.t

    val join : t -> t -> t
    val meet : t -> t -> t

    val fresh : (elt -> t) -> t

    val extract : t -> elt Stream.t
  end
