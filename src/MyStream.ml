type 'a t =
  | Nil
  | Cons of 'a * 'a t
  | Thunk of (unit -> 'a t)

(* construction *)

let empty = Nil

let single x = Cons (x, Nil)

let delay zz = Thunk zz

let rec of_list = function
  | []    -> Nil
  | x::xs -> Cons (x, of_list xs)

(* combinators *)

let rec map f = function
  | Nil           -> Nil
  | Cons (x, xs)  -> Cons (f x, map f xs)
  | Thunk zz      -> delay @@ fun () -> map f (zz ())

let rec filter p = function
  | Nil           -> Nil
  | Cons (x, xs)  -> let xs = filter p xs in if p x then Cons (x, xs) else xs
  | Thunk zz      -> delay @@ fun () -> filter p (zz ())

let rec zip xs ys =
  match xs, ys with
  | Nil         , Nil          -> Nil
  | Cons (x, xs), Cons (y, ys) -> Cons ((x, y), delay @@ fun () -> zip xs ys)
  | _           , Thunk zz     -> delay @@ fun () -> zip xs (zz ())
  | Thunk zz    , _            -> delay @@ fun () -> zip (zz ()) ys
  | _                          -> invalid_arg "Stream.zip: streams have different lengths"

let rec concat xs ys =
  match xs with
  | Nil           -> ys
  | Cons (x, xs)  -> Cons (x, delay @@ fun () -> concat xs ys)
  | Thunk zz      -> delay @@ fun () -> concat (zz ()) ys

let rec merge f xs ys =
  match xs with
  | Nil           -> Nil
  | Cons (x, xs)  -> concat (map (f x) ys) (delay @@ fun () -> merge f xs ys)
  | Thunk zz      -> delay @@ fun () -> merge f (zz ()) ys

let rec interleave xs ys =
  match xs with
  | Nil           -> ys
  | Cons (x, xs)  -> Cons (x, delay @@ fun () -> interleave ys xs)
  | Thunk zz      -> delay @@ fun () -> interleave ys (zz ())

let rec interweave f xs ys =
  match xs with
  | Nil           -> Nil
  | Cons (x, xs)  -> interleave (map (f x) ys) (delay @@ fun () -> interweave f ys xs)
  | Thunk zz      -> delay @@ fun () -> interweave f ys (zz ())

(* extraction *)

let rec split = function
  | Nil            -> None
  | Cons (x, xs)   -> Some (x, xs)
  | Thunk zz       -> split @@ zz ()

let hd s =
  match split s with
  | Some (x, _) -> x
  | None        -> invalid_arg "Stream.hd: empty stream"

let tl s =
  match split s with
  | Some (_, xs) -> xs
  | None         -> Nil

let rec retrieve ?(n=(-1)) s =
  if n = 0
  then [], s
  else match split s with
  | None          -> [], Nil
  | Some (x, s)  -> let xs, s = retrieve ~n:(n-1) s in x::xs, s

let take ?n s = fst @@ retrieve ?n s

(* iteration *)

let rec iter f s =
  match split s with
  | Some (x, s) -> f x; iter f s
  | None        -> ()

let rec fold f acc s =
  match split s with
  | Some (x, s) -> fold f (f acc x) s
  | None        -> acc

(* tests *)

let is_empty xs =
  match split xs with
  | Some _  -> false
  | None    -> true

(* monad *)

let return = single

let rec bind xs f =
  match split xs with
  | None          -> Nil
  | Some (x, xs)  -> concat (delay @@ fun () -> f x) (delay @@ fun () -> bind xs f)

let rec bindfair xs f =
  match split xs with
  | None          -> Nil
  | Some (x, xs)  -> interleave (delay @@ fun () -> f x) (delay @@ fun () -> bind xs f)
