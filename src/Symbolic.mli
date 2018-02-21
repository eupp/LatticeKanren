module Var :
  sig
    type t

    val fresh : unit -> t

    val show : t -> string

    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

module VarMap : module type of Map.Make(Var)

module type Value =
  sig
    type t
    (* include Lattice.T *)

    val injvar : Var.t -> t

    val is_var : t -> Var.t option

    val show : t -> string

    val equal : t -> t -> bool
  end

module type Solver =
  sig
    type t

    module Domain : Value

    (* include Lattice.T *)

    (* val extract : Domain.t -> t -> Domain.t MyStream.t *)
  end

module type EqSolver =
  sig
    include Solver

    (* val eq : Domain.t -> Domain.t -> t *)
    val (===) : Domain.t -> Domain.t -> t
  end
