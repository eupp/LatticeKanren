module Make(T : Lattice.T) =
  struct
    type t = T.t -> (T.t * t) option

    let bot = fun _ -> None
    let top = fun a -> Some (a, bot)

    let delay g =
      fun a -> g () a

    let is_bot g =
      match g T.top with
      | None   -> true
      | Some _ -> false

    let is_top g =
      match g T.top with
      | None          -> false
      | Some (a, tl)  -> (T.is_top a) && (is_bot tl)

    let lift a =
      if T.is_bot a then bot else fun b ->
        match T.meet a b with
        | c when T.is_bot c -> None
        | c                 -> Some (c, bot)

    let rec (<|>) g g' = fun a ->
      match g a with
      | None          -> g' a
      | Some (b, tl)  -> Some (b, fun a' ->
        match T.meet a a' with
        | c when T.is_bot c -> None
        | c                 -> (g' <|> tl) c
      )

    let rec (<&>) g g' = fun a ->
      match g a with
      | None          -> None
      | Some (b, tl)  ->
        match g' b with
        | None          -> (g' <&> tl) a
        | Some (c, tl') -> Some (c, (g' <&> tl) <|> tl')

    let rec run ?(n=(-1)) g =
      if (n=0) then [] else
        match g T.top with
        | None         -> []
        | Some (b, tl) -> let bs = run ~n:(n-1) tl in b::bs

  end
