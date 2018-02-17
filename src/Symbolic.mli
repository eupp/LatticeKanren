module Var :
  sig
    type t

    val make : int -> t

    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

module VarMap : module type of Map.Make(Var)

module type Value =
  sig
    include Lattice.T

    val injvar : Var.t -> t

    val is_var : t -> Var.t option
  end

module type Solver =
  sig
    module Domain : Value

    include Lattice.T

    val extract : t -> Domain.t Stream.t
  end

module type EqSolver =
  sig
    include Solver

    val eq : Domain.t -> Domain.t -> t
  end
