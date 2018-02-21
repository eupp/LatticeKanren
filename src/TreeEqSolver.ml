open Symbolic

(* module Make (T : EqSolver) =
  struct *)
    type tree =
      | Var of Var.t
      | Func of string * tree list
      (* | Const of T.Domain.t *)
      (* | Bot *)

    type t = tree VarMap.t option

    let top = Some VarMap.empty
    let bot = None

    let is_top = function
      | Some s -> (VarMap.is_empty s)
      | None   -> false

    let is_bot = function
      | Some _ -> false
      | None   -> true

    exception Occurs_check
    exception Unification_failed

    let rec walk x s =
      match x with
      | Var v -> (try walk (VarMap.find v s) s with Not_found -> x)
      | _     -> x

    let rec reify x s =
      match walk x s with
      | Var v        -> Var v
      | Func (f, xs) -> Func (f, List.map (fun x -> reify x s) xs)

    let rec occurs_exn v x s =
      match walk x s with
      | Var u         -> if Var.equal v u then raise Occurs_check else ()
      | Func (f, xs)  -> List.iter (fun x -> occurs_exn v x s) xs
      (* | _             -> () *)

    let extend v x s =
      occurs_exn v x s;
      VarMap.add v x s

    let rec unify x y t =
      try
        match t with
        | None   -> None
        | Some s ->
        (* if is_bot t then bot else *)
          match walk x s, walk y s with
          | Var v, Var u ->
            if Var.equal u v then t else Some (extend u (Var v) s)
          | Var v, x | x, Var v ->
            begin match x with
            | Func _  -> Some (extend v x s)
            (* | Const c -> Some (extend v x s) *)
            (* | Bot     -> raise Unification_failed *)
            end
          (* | Const a, Const b -> (s, T.(meet t @@ eq a b)) *)
          (* | Const a, Const b  *)
          | Func (f, xs), Func (g, ys) when f = g ->
            ListLabels.fold_left2 xs ys ~init:t ~f:(fun acc x y -> unify x y acc)
          | _ -> raise Unification_failed
      with Occurs_check | Unification_failed -> bot

    let (===) x y = unify x y top

    (* let rec antiunify x y =
      match x, y with
      | Bot, _ -> y
      | _, Bot -> x
      | Func (f, xs), Func (g, ys) when f = g -> Func (f, List.map2 antiunify xs ys)
      | _ -> Var (Var.fresh ()) *)

    let meet a b =
      match a, b with
      | None,_ | _,None -> None
      | Some s, Some s' ->
        let s, s' = if VarMap.(cardinal s < cardinal s') then s, s' else s', s in
        VarMap.fold (fun v x -> unify (Var v) x) s (Some s')

      (* if (is_bot a) || (is_bot b) then bot else
        let (s, t), (s', t') = a, b in
        let t = T.meet t t' in
        if T.is_bot t then bot else
          let s, s' = if VarMap.(cardinal s < cardinal s') then s, s' else s', s in
          VarMap.fold (fun v x -> unify (Var v) x) s (s', t) *)

    let join a b =
      failwith "Not implemented"
      (* if (is_top a) || (is_top b) then top else
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
        (s, T.join t t') *)

    let fresh g =
      let v = Var (Var.fresh ()) in g v

    let rec extract x t =
      match t with
      | None   -> MyStream.empty
      | Some s -> MyStream.single (reify x s)

    (* let rec extract x ((s, t) as solver) =
      if is_bot solver then Stream.empty else
        match walk x s with
        | Bot             -> Stream.empty
        | Var v as x      -> Stream.single x
        | Const c         -> Stream.map (fun c -> Const c) @@ T.extract c t
        | Func (f, xs)    ->
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
          Stream.map (fun xs -> Func (f, xs)) stream *)

    module Domain =
      struct
        type t = tree

        (* let top' = top *)

        (* let top = Var (Var.dummy) *)

        (* let bot = Bot *)

        (* let is_top = function
        | Var _ -> true
        | _     -> false *)

        (* let is_bot = function
        | Bot -> true
        | _   -> false *)

        (* let join = antiunify *)

        (* let meet x y =
          let solver = unify x y top' in
          match Stream.split @@ extract x solver with
          | Some (x, xs) -> assert (Stream.is_empty xs); x
          | None         -> assert false *)

        let injvar v = Var v

        let is_var = function
        | Var v -> Some v
        | _     -> None

        let rec show = function
        | Var v         -> Var.show v
        | Func (f, xs)  -> Printf.sprintf "%s (%s)" f (String.concat ", " @@ List.map show xs)

        let rec equal x y =
          match x, y with
          | Var v, Var u        -> Var.equal v u
          | Var _, _ | _, Var _ -> false
          | Func (f,xs), Func (g,ys) when f=g -> List.for_all2 equal xs ys
          | _ -> false

      end

    let show = function
    | None    -> "bot"
    | Some s  ->
      let pp v x = Printf.sprintf "%s=%s" (Var.show v) (Domain.show x) in
      let bs = VarMap.fold (fun v x acc -> (pp v x)::acc) s [] in
      Printf.sprintf "subst {%s}" @@ String.concat "; " bs

  (* end *)
