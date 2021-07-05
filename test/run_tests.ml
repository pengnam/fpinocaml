(*
   Run all the OCaml test suites defined in the fpinocamlect.
*)

let test_suites : unit Alcotest.test list =
  [
    ("Chapter2.A", Test_chapter2.A.tests);
    ("Chapter3.A", Test_chapter3.A.tests);
    ("Chapter4.A", Test_chapter4.A.tests);
    ("Chapter4.B", Test_chapter4.B.tests);
    ("Chapter4.C", Test_chapter4.C.tests);
    ("Chapter5.A", Test_chapter5.A.tests);
    ("Chapter6.A", Test_chapter6.A.tests);
    ("Chapter7.A", Test_chapter7.A.tests);
    ("Chapter8.A", Test_chapter8.A.tests);
    ("Chapter10.monoid", Test_chapter10.monoid.tests);
  ]

let () = Alcotest.run "fpinocaml" test_suites
