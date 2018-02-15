open OUnit2

let tests = "lkanren" >:::
  [ "stream" >::: [TestStream.tests]

  ]

let () =
  run_test_tt_main tests
