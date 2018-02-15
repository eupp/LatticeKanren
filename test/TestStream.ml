open OUnit2
open Lkanren.Stream


let nats =
  let rec helper i = concat (return i) (delay (fun () -> helper (i+1))) in
  helper 1

let printer xs = String.concat ", " @@ List.map string_of_int xs

let tests =
  "stream" >:::
    [ "split" >:: (fun test_ctx ->
        match split nats with
        | None         -> assert_failure "empty stream"
        | Some (n, ns) -> assert_equal 1 n
      )

    ; "take" >:: (fun test_ctx ->
        let ns = take ~n:3 nats in
        assert_equal [1;2;3] ns
      )

    ; "concat" >:: (fun test_ctx ->
        let s = concat (of_list [1;2;3]) (of_list [4;5;6]) in
        let xs = take s in
        assert_equal [1;2;3;4;5;6] xs
      )

    ; "interleave" >:: (fun test_ctx ->
        let s = interleave (of_list [1;2;3]) (of_list [4;5;6]) in
        let xs = take s in
        assert_equal [1;4;2;5;3;6] xs
      )

    ; "concat-inf" >:: (fun test_ctx ->
        let s = concat nats nats in
        let xs = take ~n:6 s in
        assert_equal [1;2;3;4;5;6] xs
      )

    ; "interleave-inf" >:: (fun test_ctx ->
        let s = interleave nats nats in
        let xs = take ~n:6 s in
        assert_equal ~printer [1;1;2;2;3;3] xs
      )

    ; "merge" >:: (fun test_ctx ->
        let s = merge (+) (of_list [1;2;3]) (of_list [4;5;6]) in
        let xs = take s in
        assert_equal [5;6;7;6;7;8;7;8;9] xs
      )

    ; "interweave" >:: (fun test_ctx ->
        let s = interweave (+) (of_list [1;2;3]) (of_list [4;5;6]) in
        let xs = take s in
        assert_equal ~printer [5;6;6;7;7;7;8;8;9] xs
      )

    ; "merge-inf" >:: (fun test_ctx ->
        let s = merge (+) nats nats in
        let xs = take ~n:6 s in
        assert_equal [2;3;4;5;6;7] xs
      )

    ; "interweave-inf" >:: (fun test_ctx ->
        let s = interweave (+) nats nats in
        let xs = take ~n:6 s in
        assert_equal ~printer [2;3;3;4;5;6] xs
      )

    ; "interweave-inf-empty" >:: (fun test_ctx ->
        let s = interweave (+) nats empty in
        let xs = take s in
        assert_equal xs []
      )

    ; "bind-inf" >:: (fun test_ctx ->
        let s = bind (of_list [1;2;3]) (fun x -> map (fun y -> string_of_int @@ x + y) nats) in
        let xs = take ~n:6 s in
        assert_equal ~printer:(String.concat "; ") ["2";"3";"4";"5";"6";"7"] xs
      )

    ; "bindfair-inf" >:: (fun test_ctx ->
        let s = bindfair (of_list [1;2;3]) (fun x -> map (fun y -> string_of_int @@ x + y) nats) in
        let xs = take ~n:6 s in
        assert_equal ~printer:(String.concat "; ") ["2";"3";"3";"4";"4";"5"] xs
      )

    ]
