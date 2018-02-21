(* type ('a, 'b) t = 'a -> ('b option * ('a, 'b) t) option *)

(* type ('a, 'b) t =
  | Co of ('a -> ('b * ('a, 'b) t) option)
  | Thunk of (unit -> ('a, 'b) t) *)

type ('a, 'b) t =
  'a -> ('a, 'b) stream
and ('a, 'b) stream =
  | Nil
  | Cons of ('b * ('a, 'b) t)
  | Thunk of ('a, 'b) t * 'a

(* monad+ *)

let empty = fun _ -> Nil

let rec force = function
  | Thunk (zz, x) -> zz x
  | s -> s

(* let rec interleave g g' a =
  match force (g a) with
  | Nil          -> g' a
  | Cons (b, tl) -> Cons (b, interleave g' tl) *)

let rec interleave g g' a =
  match force (g a) with
  | Nil          -> g' a
  | Cons (b, tl) -> Cons (b, fun _ -> Thunk (interleave g' tl, a))

(* let rec interweave g g' a =
  match force (g a) with
  | Nil          -> Nil
  | Cons (b, tl) ->
    match force (g' b) with
    | Nil           -> Nil
    (* | Cons (c, tl') -> Cons (c, interleave (interweave g' tl) tl') *)
    | Cons (c, tl') -> Cons (c, interweave tl' tl) *)

let rec interweave g g' a =
  match force (g a) with
  | Nil          -> Nil
  | Cons (b, tl) ->
    match force (g' b) with
    | Nil           -> Nil
    (* | Cons (c, tl') -> Cons (c, interleave (interweave g' tl) tl') *)
    | Cons (c, tl') -> Cons (c, fun _ -> Thunk (interweave tl g', a))

let rec map g f a =
  match force (g a) with
  | Nil          -> Nil
  | Cons (b, tl) -> Cons (f b, map g f)

let rec map_opt g f a =
  match force (g a) with
  | Nil          -> Nil
  | Cons (b, tl) ->
    match f b with
    | Some c -> Cons (c, map_opt g f)
    | None   -> Nil

(* monad *)

(* let return b = fun _ -> Cons (b, empty)

let rec bind g f a =
  match force (g a) with
  | Nil          -> Nil
  | Cons (b, tl) -> Thunk (interleave (f b) (bind tl f), a) *)

(* run *)

let rec run ?(n=(-1)) a g =
  if (n=0) then [] else
    match force (g a) with
    | Nil          -> []
    | Cons (b, tl) -> let bs = run ~n:(n-1) a tl in b::bs

(*  *)

(*  *)

let lift_f f a = Cons (f a, empty)

let lift_fopt f a =
  match f a with
  | None    -> Nil
  | Some b  -> Cons (b, empty)

(* delay *)

let delay g x = Thunk (g (), x)

let delay3 g a b c x = Thunk (g a b c, x)
