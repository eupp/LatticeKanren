module T :
  sig
    type t

    val top : t
    val bot : t

    val join : t -> t -> t
    val meet : t -> t -> t
  end
