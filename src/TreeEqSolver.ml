open Symbolic

module Make (T : EqSolver) =
  struct
    type tree =
      | Var of Var.t
      | Const of T.Domain.t
      | Func of string * tree list
      | Bot

    type t = tree VarMap.t * T.t

    let top = (VarMap.empty, T.top)
    let bot = (VarMap.empty, T.bot)

    let is_top (s, t) = (VarMap.is_empty s) && (T.is_top t)
    let is_bot (s, t) = (T.is_bot t)

    exception Occurs_check
    exception Unification_failed

    let rec walk x s =
      match x with
      | Var v -> try walk (VarMap.find v s) s with Not_found -> x
      | _     -> x

    let rec occurs_exn v x s =
      match x with
      | Var u         -> if Var.equal v u then raise Occurs_check else ()
      | Func (f, xs)  -> List.iter (fun x -> occurs_exn v x s) xs
      | _             -> ()

    let rec unify x y ((s, t) as solver) =
      let extend v t s =
        occurs_exn v t s;
        VarMap.add v t s
      in
      try
        if is_bot solver then bot else
          match walk x s, walk y s with
          | Var v, Var u ->
            if Var.equal u v then solver else (extend u (Var v) s, t)
          | Var v, x | x, Var v ->
            begin match x with
            | Func _  -> (extend v x s, t)
            | Const c -> (extend v x s, T.(meet t @@ eq c (T.Domain.injvar v)))
            | Bot     -> raise Unification_failed
            end
          | Const a, Const b -> (s, T.(meet t @@ eq a b))
          | Func (f, xs), Func (g, ys) when f = g ->
            ListLabels.fold_left2 xs ys ~init:solver ~f:(fun acc x y -> unify x y acc)
          | _ -> raise Unification_failed
      with Occurs_check | Unification_failed -> bot

    let rec antiunify x y =
      match x, y with
      | Bot, _ -> y
      | _, Bot -> x
      | Func (f, xs), Func (g, ys) when f = g -> Func (f, List.map2 antiunify xs ys)
      | _ -> Var (Var.make (-1))

    let meet a b =
      if (is_bot a) || (is_bot b) then bot else
        let (s, t), (s', t') = a, b in
        let t = T.meet t t' in
        if T.is_bot t then bot else
          let s, s' = if VarMap.(cardinal s < cardinal s') then s, s' else s', s in
          VarMap.fold (fun v x -> unify (Var v) x) s (s', t)

    let join a b =
      if (is_top a) || (is_top b) then top else
      if (is_bot a) then b else
      if (is_bot b) then a else
        let (s, t), (s', t') = a, b in
        let s, s' = to_idempotent s, to_idempotent s' in
        let s, s' = if VarMap.(size s < size s') then s, s' else s', s in
        let s = VarMap.fold (fun var x acc ->
          try
            let y = VarMap.find var s' in
            VarMap.add var (antiunify x y) acc
          with Not_found -> acc
        ) s VarMap.empty
        in
        (s, T.join t t')

    let extract x ((s, t) as solver) =
      if is_bot solver then Stream.empty else
        match walk x s with
        | Bot             -> Stream.empty
        | Var v as x      -> x
        | Const c         -> Stream.map (fun c -> Const c) @@ T.extract c t
        | Func (f, x::xs) ->
          let stream = ListLabels.fold_right xs
            ~init:(Stream.return [])
            ~f:(fun x acc ->
              Stream.bindfair acc (fun xs ->
                Stream.bindfair (extract x solver) (fun x ->
                  Stream.return x::xs
                )
              )
            )
          in
          Stream.map (fun xs -> Func (f, xs)) stream

    module Domain =
      struct
        type t = tree

        let top' = top

        let top = Var (Var.of_int 0)

        let bot = Bot

        let is_top = function
        | Var _ -> true
        | _     -> false

        let is_bot = function
        | Bot -> true
        | _   -> false

        let join = antiunify

        let meet x y =
          let solver = unify x y top' in
          match Stream.split @@ extract x solver with
          | Some (x, xs) -> assert (Stream.is_empty xs); x
          | None         -> assert false

        let injvar v = Var v

        let is_var = function
        | Var v -> Some v
        | _     -> None

      end

  end
