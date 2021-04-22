(*
   Tests for Chapter2.
*)
let test_fib () =
    let check_fib msg input expected = Alcotest.(check bool) msg true (Fpinocaml_chapter2.A.fib input = expected) in
  check_fib "fib 1" 1 1;
  check_fib "fib 12" 12 144

let tests = [ ("fib", `Quick, test_fib) ]
