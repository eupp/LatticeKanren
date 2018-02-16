module Make (T : Symbolic.EqSolver) :
  sig
    module Domain : module type of Tree.Make(T.Domain)

    include Lattice.T

    val fresh : (Domain.t -> t) -> t

    val extract : t -> Domain.t Stream.t
  end
