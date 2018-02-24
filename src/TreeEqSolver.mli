
type tree =
  | Var of Symbolic.Var.t
  | Func of string * tree list

module Domain : Symbolic.Value with type t = tree

include Lattice.T

val (===) : Domain.t -> Domain.t -> t

val fresh : (Domain.t -> t) -> t

val reify : Domain.t -> t -> Domain.t option

val show : t -> string
