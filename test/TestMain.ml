open OUnit2

let tests = "lkanren" >:::
  [ TestStream.tests
  ; TestTreeEqSolver.tests
  ]

let () =
  run_test_tt_main tests
