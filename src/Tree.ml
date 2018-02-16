module Make (T : Symbolic.Value) =
  struct
    type t =
      | Var of Var.t
      | Leaf of T.t
      | Node of string * T.t list

    (* construction *)

    let leaf x = Leaf x
    let node ctr xs = Node (ctr, xs)

    (* var stuff *)

    let injvar v = Var v

    let is_var = function
    | Var v -> Some v
    | _     -> None

    (* traversal *)

    let map ~fvar ~fval = function
    | Var v           -> fvar v
    | Leaf x          -> fval x
    | Node (ctr, xs)  -> Node (ctr, List.map (map ~fvar ~fval) xs)

    let fold ~fvar ~fval ~init = function
    | Var v           -> fvar v init
    | Leaf x          -> fval x init
    | Node (ctr, xs)  -> ListLabels.fold_left xs ~init
      ~f:(fun acc x -> fold x ~fvar ~fval ~init:acc)

    exception Different_shape

    let fold2 ~fvar ~fval ~fk ~init x y =
      match x, y with
      | Var v, Var u    -> fvar v u init
      | Var v, _        -> fk v y init
      | _    , Var u    -> fk u x init
      | Leaf x, Leaf y  -> fval x y init
      | Node (ctr1, xs), Node (ctr2, ys) ->
        if (ctr1 = ctr2) && (List.(length xs = length ys)) then
          ListLabels.fold_left2 xs ys ~init
            ~f:(fun acc x y -> fold2 x y ~fvar ~fval ~fk ~init:acc)
        else
          raise Different_shape

  end
