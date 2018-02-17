module Make (T : Symbolic.EqSolver) :
  sig
    type tree =
      | Var of Symbolic.Var.t
      | Const of T.t
      | Func of string * T.t list
      | Bot

    module Domain : Symbolic.Value with type t = tree

    include Lattice.T

    val fresh : (Domain.t -> t) -> t

    val extract : t -> Domain.t Stream.t
  end
