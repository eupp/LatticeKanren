open OUnit2
open Lkanren.Symbolic
open Lkanren.TreeEqSolver
open Lkanren.MyStream

let printer = Domain.show

let tests =
  "TreeEqSolver" >:::
    [ "unify-var-same" >:: (fun test_ctx ->
        let v = Var (Var.fresh ()) in
        let s = (v === v) in
        assert_bool "get not a top of lattice" (is_top s)
      )

    ; "unify-var-var" >:: (fun test_ctx ->
          let v = Var (Var.fresh ()) in
          let u = Var (Var.fresh ()) in
          let s = (v === u) in
          let answs = extract (Func ("pair", [v; u])) s in
          let [answ] = take answs in
          match answ with
          | Func ("pair", [Var a; Var b]) ->
            assert_bool "Vars not equal" (Var.equal a b)
          | _ -> assert_failure ""
        )

    ; "unify-var-term" >:: (fun test_ctx ->
          let v = Var (Var.fresh ()) in
          let t = Func ("f", []) in
          let s = (v === t) in
          let answs = extract v s in
          let [answ] = take answs in
          assert_equal t answ
        )

    ; "unify-term-diff" >:: (fun test_ctx ->
          let t  = Func ("f", []) in
          let t' = Func ("g", []) in
          let s = (t === t') in
          let answs = extract t s in
          assert_bool "Stream not empty" (is_empty answs)
        )

    ; "unify-term-term" >:: (fun test_ctx ->
          let t  = Func ("Cons", [Func ("x", []);     Var (Var.fresh ())]) in
          let t' = Func ("Cons", [Var (Var.fresh ()); Func ("Nil", []); ]) in
          let s = (t === t') in
          let answs = extract t s in
          let [answ] = take answs in
          assert_equal (Func ("Cons", [Func ("x", []); Func ("Nil", [])])) answ
        )

    ; "meet" >:: (fun test_ctx ->
          let v  = Var (Var.fresh ()) in
          let t  = Func ("Cons", [Func ("x", []);     Var (Var.fresh ())]) in
          let t' = Func ("Cons", [Var (Var.fresh ()); Func ("Nil", []); ]) in
          let s = meet (t === t') (v === t) in
          let answs = extract v s in
          let [answ] = take answs in
          assert_equal ~printer (Func ("Cons", [Func ("x", []); Func ("Nil", [])])) answ
        )

    ; "meet-bot" >:: (fun test_ctx ->
          let v  = Var (Var.fresh ()) in
          let t  = Func ("f", []) in
          let t' = Func ("g", []) in
          let s = meet (v === t) (v === t') in
          let answs = extract v s in
          assert_bool "Stream not empty" (is_empty answs)
        )

    ]
