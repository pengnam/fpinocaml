(*
   Run all the OCaml test suites defined in the fpinocamlect.
*)

let test_suites : unit Alcotest.test list =
  [
    ("Chapter2.A", Test_chapter2.A.tests);
  ]

let () = Alcotest.run "fpinocaml" test_suites
