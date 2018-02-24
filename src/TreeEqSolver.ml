open Symbolic

type tree =
  | Var of Var.t
  | Func of string * tree list

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

let rec reify x t =
  let rec helper x s =
    match walk x s with
    | Var v        -> Var v
    | Func (f, xs) -> Func (f, List.map (fun x -> helper x s) xs)
  in
  match t with
  | None    -> None
  | Some s  -> Some (helper x s)

let rec occurs_exn v x s =
  match walk x s with
  | Var u         -> if Var.equal v u then raise Occurs_check else ()
  | Func (f, xs)  -> List.iter (fun x -> occurs_exn v x s) xs

let extend v x s =
  occurs_exn v x s;
  VarMap.add v x s

let rec unify x y t =
  try
    match t with
    | None   -> None
    | Some s ->
      match walk x s, walk y s with
      | Var v, Var u ->
        if Var.equal u v then t else Some (extend u (Var v) s)
      | Var v, x | x, Var v ->
        begin match x with
        | Func _  -> Some (extend v x s)
        end
      | Func (f, xs), Func (g, ys) when f = g ->
        ListLabels.fold_left2 xs ys ~init:t ~f:(fun acc x y -> unify x y acc)
      | _ -> raise Unification_failed
  with Occurs_check | Unification_failed -> bot

let (===) x y = unify x y top

let meet a b =
  match a, b with
  | None,_ | _,None -> None
  | Some s, Some s' ->
    let s, s' = if VarMap.(cardinal s < cardinal s') then s, s' else s', s in
    VarMap.fold (fun v x -> unify (Var v) x) s (Some s')

let join a b = failwith "Not implemented"

let fresh g = let v = Var (Var.fresh ()) in g v

module Domain =
  struct
    type t = tree

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
