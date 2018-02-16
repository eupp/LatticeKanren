open Symbolic

module Make (Inner : EqSolver) =
  struct
    module Term = Tree.Make(Inner.Domain)

    module Domain = Term

    type t = Bot | Sbs of Term.t VarMap.t * Inner.t

    let top = Sbs (VarMap.empty, Inner.top)
    let bot = Bot

    let is_top = function
    | Sbs (s, slvr) -> (VarMap.is_empty s) && (Inner.is_top slvr)
    | Bot           -> false

    let is_bot = function
    | Bot   -> true
    | _     -> false

    let meet_inner s a b =
      let inner = Inner.meet a b in
      if Inner.is_bot inner then Bot else (s, inner)

    let unify x y s =
      let extend v t s =
        occurs_exn v t;
        VarMap.add v t s
      in
      let rec helper x y acc = Term.(fold2 x y ~init:acc
        ~fvar:(fun v u acc -> match acc with
          | Bot             -> Bot
          | Sbs (s, slvr)   ->
            match Subst.(walk s v, walk s u) with
            | Var u, Var v ->
              if Var.equal u v then acc else Sbs (extend u (Term.injvar v) s, slvr)
            | Var v, t | t, Var v ->
              begin match t with
              | Node _ -> Sbs (extend v t s, slvr)
              | Leaf t -> meet_inner s slvr (Inner.eq t @@ Inner.injvar v)
              end
            | x, y -> helper x y acc
        )
        ~fval:(fun x y (s, slvr) ->
          match acc with
          | Bot           -> Bot
          | Sbs (s, slvr) -> meet_inner s slvr (Inner.eq x y)
        )
        ~fk:(fun v y ((s, slvr) as acc) ->
          match acc with
          | Bot           -> Bot
          | Sbs (s, slvr) ->
            match walk s v with
            | Var v -> (extend v t s, slvr)
            | x     -> helper x y acc
        )
      in
      try helper x y s with Term.Different_shape | Occurs_check -> Bot

    let meet a b =
      match a, b with
      | Bot, _ -> Bot
      | _, Bot -> Bot
      | Sbs (s, inner), Sbs (s', inner') ->
        let s, inner, init = if VarMap.(size s < size s') then s, inner, b else s', inner', a in
        match VarMap.fold (fun v t -> unify (Term.injvar v) t) s init with
        | Bot              -> Bot
        | Sbs (s', inner') -> meet_inner s' inner inner'



  end
