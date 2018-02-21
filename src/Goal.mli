(* Goal is a suspended non-deterministic computation (possible infinite) *)
type ('a, 'b) t

(* monad on the second type parameter *)

val return : 'b -> ('a, 'b) t

val bind : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t

(* monad+ on the second type parameter with different `mplus` strategies *)

val empty : ('a, 'b) t

(* interleaving of two goals *)
val interleave : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

(* interweaving of two goals *)
val interweave : ('a, 'a) t -> ('a, 'a) t -> ('a, 'a) t

(*  *)

val run : ?n:int -> 'a -> ('a, 'b) t -> 'b list

val delay : (unit -> ('a, 'b) t) -> ('a, 'b) t

val delay3 : ('k -> 'l -> 'm -> ('a, 'b) t) -> 'k -> 'l -> 'm -> ('a, 'b) t

val lift_f : ('a -> 'b) -> ('a, 'b) t

val lift_fopt : ('a -> 'b option) -> ('a, 'b) t
