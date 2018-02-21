type 'a t

(* construction *)

val empty : 'a t

val single : 'a -> 'a t

val of_list : 'a list -> 'a t

val delay : (unit -> 'a t) -> 'a t

(* monad *)

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val bindfair : 'a t -> ('a -> 'b t) -> 'b t

(* combinators *)

val map : ('a -> 'b) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

val concat : 'a t -> 'a t -> 'a t

val interleave : 'a t -> 'a t -> 'a t

val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val interweave : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val zip : 'a t -> 'b t -> ('a * 'b) t

(* tests *)

val is_empty : 'a t -> bool

(* iteration *)

val iter : ('a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(* extraction *)

val split : 'a t -> ('a * 'a t) option

val retrieve : ?n:int -> 'a t -> 'a list * 'a t

val take : ?n:int -> 'a t -> 'a list

val hd : 'a t -> 'a

val tl : 'a t -> 'a t
