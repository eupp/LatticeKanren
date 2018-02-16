module Make (T : Symbolic.Value) :
  sig
    type t

    (* construction *)

    val leaf : T.t -> t
    val node : string -> T.t list -> t

    (* var stuff *)

    val injvar : Var.t -> t
    val is_var : t -> Var.t option

    (* traversal *)

    val map : fvar:(Var.t -> t) -> fval:(T.t -> t) -> t -> t

    val fold : fvar:(Var.t 'a -> 'a) -> fval:(T.t -> 'a -> 'a) -> init:'a -> t -> 'a

    exception Different_shape

    val fold2 :
      fvar:(Var.t 'a -> 'a) -> fval:(T.t -> 'a -> 'a) -> fk:(Var.t -> t -> 'a -> 'a) ->
      init:'a -> t -> 'a
  end
