type ('a, 'b) t = 'a -> ('b option * ('a, 'b) t) option

(* monad+ *)

let empty = fun _ -> None

let rec interleave g g' a =
  match g a with
  | None          -> g' a
  | Some (b, tl)  -> Some (b, interleave g' tl)

let rec interweave g g' a =
  match g a with
  | None          -> None
  | Some (b, tl)  ->
    match b with
    | None    -> interweave tl g' a
    | Some b  ->
      match g' b with
      | None          -> None
      | Some (c, tl') -> Some (c, interleave (interweave g' tl) tl')

(* monad *)

let return b = fun _ -> Some (Some b, empty)

let rec bind g f a =
  match g a with
  | None          -> None
  | Some (b, tl)  ->
    match b with
    | None    -> bind tl f a
    | Some b  -> Some (None, interleave (f b) (bind tl f))

(* run *)

let rec run ?(n=(-1)) a g =
  if n=0 then [] else
    match g a with
    | None          -> []
    | Some (b, tl)  ->
      match b with
      | None   -> run ~n a g
      | Some b ->
        let bs = run ~n:(n-1) a g in b::bs

(*  *)

let lift_f f a = Some (Some (f a), empty)

let lift_fopt f a =
  match f a with
  | None    -> None
  | Some b  -> Some (Some b, empty)
