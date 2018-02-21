(* module Make (T : Symbolic.EqSolver) :
  sig *)
    type tree =
      | Var of Symbolic.Var.t
      | Func of string * tree list
      (* | Const of T.t *)
      (* | Bot *)

    module Domain : Symbolic.Value with type t = tree

    include Lattice.T

    val fresh : (Domain.t -> t) -> t

    val extract : Domain.t -> t -> Domain.t Stream.t
  (* end *)
