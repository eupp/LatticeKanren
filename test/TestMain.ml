open OUnit2

let tests = "lkanren" >:::
  [ TestStream.tests
  ; TestTreeEqSolver.tests
  ; TestAppendo.tests
  ]

let () =
  run_test_tt_main tests
