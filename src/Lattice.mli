module type T :
  sig
    type t

    val top : t
    val bot : t

    val is_top : t -> bool
    val is_bot : t -> bool

    val join : t -> t -> t
    val meet : t -> t -> t
  end
