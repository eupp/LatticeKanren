module Make(T : Lattice.T) =
  struct
    type t = T.t -> (T.t * t) option

    let empty = fun _ -> None

    let delay g =
      fun a -> g () a

    let lift a =
      if T.is_bot a then empty else fun b ->
        match T.meet a b with
        | c when T.is_bot c -> None
        | c                 -> Some (c, empty)

    let rec (|>>) g g' = fun a ->
      match g a with
      | None          -> g' a
      | Some (b, tl)  -> Some (b, fun a' ->
        match T.meet a a' with
        | c when T.is_bot c -> None
        | c                 -> (tl |>> g') c
      )

    let rec (<|>) g g' = fun a ->
      match g a with
      | None          -> g' a
      | Some (b, tl)  -> Some (b, fun a' ->
        match T.meet a a' with
        | c when T.is_bot c -> None
        | c                 -> (g' <|> tl) c
      )

    let rec (&>>) g g' = fun a ->
      match g a with
      | None          -> None
      | Some (b, tl)  ->
        match g' b with
        | None          -> (tl &>> g') a
        | Some (c, tl') -> Some (c, (tl &>> g') |>> tl')

    let rec (<&>) g g' = fun a ->
      match g a with
      | None          -> None
      | Some (b, tl)  ->
        match g' b with
        | None          -> (g' <&> tl) a
        | Some (c, tl') ->
          let g' = fun a' -> match T.meet a a' with
            | d when T.is_bot d -> None
            | d                 -> g' d
          in
          Some (c, (g' <&> tl) <|> tl')

    let rec run ?(n=(-1)) g =
      if (n=0) then [] else
        match g T.top with
        | None         -> []
        | Some (b, tl) -> let bs = run ~n:(n-1) tl in b::bs

  end
