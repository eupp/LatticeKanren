open Ocamlbuild_plugin;;
module Pack = Ocamlbuild_pack

let () = dispatch (function
 | Before_rules ->
     ()

 | After_rules ->
    ()
 | _ -> ()
)
