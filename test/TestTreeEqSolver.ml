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
          match reify (Func ("", [v; u])) s with
          | Some (Func ("", [Var a; Var b])) -> assert_bool "Vars not equal" (Var.equal a b)
          | _ -> assert_failure ""
        )

    ; "unify-var-term" >:: (fun test_ctx ->
          let v = Var (Var.fresh ()) in
          let t = Func ("f", []) in
          let s = (v === t) in
          match reify v s with
          | Some answ -> assert_equal t answ
          | None      -> assert_failure ""
        )

    ; "unify-term-diff" >:: (fun test_ctx ->
          let t  = Func ("f", []) in
          let t' = Func ("g", []) in
          let s = (t === t') in
          match reify t s with
          | None      -> ()
          | Some answ -> assert_failure ""
        )

    ; "unify-term-term" >:: (fun test_ctx ->
          let t  = Func ("Cons", [Func ("x", []);     Var (Var.fresh ())]) in
          let t' = Func ("Cons", [Var (Var.fresh ()); Func ("Nil", []); ]) in
          let s = (t === t') in
          match reify t s with
          | None      -> assert_failure ""
          | Some answ ->
            assert_equal (Func ("Cons", [Func ("x", []); Func ("Nil", [])])) answ
        )

    ; "meet" >:: (fun test_ctx ->
          let v  = Var (Var.fresh ()) in
          let t  = Func ("Cons", [Func ("x", []);     Var (Var.fresh ())]) in
          let t' = Func ("Cons", [Var (Var.fresh ()); Func ("Nil", []); ]) in
          let s = meet (t === t') (v === t) in
          match reify v s with
          | None      -> assert_failure ""
          | Some answ ->
            assert_equal (Func ("Cons", [Func ("x", []); Func ("Nil", [])])) answ
        )

    ; "meet-bot" >:: (fun test_ctx ->
          let v  = Var (Var.fresh ()) in
          let t  = Func ("f", []) in
          let t' = Func ("g", []) in
          let s = meet (v === t) (v === t') in
          match reify v s with
          | None      -> ()
          | Some answ -> assert_failure ""
        )

    ]
