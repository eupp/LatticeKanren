module Var =
  struct
    type t = int

    let next_idx = ref 0

    let fresh () =
      let idx = !next_idx in
      next_idx := !next_idx + 1;
      idx

    let equal = (=)
    let compare = compare
  end

module VarMap = Map.Make(Var)

module type Value =
  sig
    (* include Lattice.T *)
    type t

    val injvar : Var.t -> t

    val is_var : t -> Var.t option
  end

module type Solver =
  sig
    type t

    module Domain : Value

    (* include Lattice.T *)

    val extract : Domain.t -> t -> Domain.t Stream.t
  end

module type EqSolver =
  sig
    include Solver

    (* val eq : Domain.t -> Domain.t -> t *)
    val (===) : Domain.t -> Domain.t -> t
  end
