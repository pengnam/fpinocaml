(*
   Tests for Chapter2.
   *)
let test_fib () =
    let check_fib msg input expected = Alcotest.(check bool) msg true (Fpinocaml_chapter2.A.fib input = expected) in
    check_fib "fib 1" 1 1;
  check_fib "fib 12" 12 144

let test_sorted () =
    let check_sorted msg input f expected = Alcotest.(check bool) msg expected (Fpinocaml_chapter2.A.is_sorted input f) in
    check_sorted "simple sorted" [1;2;3] (<) true;
  check_sorted "reversed" [1;2;3] (>) false;
  check_sorted "empty" [] (>) true


let test_curry() = Alcotest.(check int ) "simple curry" 3 (Fpinocaml_chapter2.A.curry (fun tup -> (fst tup) + (snd tup)) 1 2 )




let tests = [ ("fib", `Quick, test_fib);("sorted", `Quick, test_sorted) ;("curry", `Quick, test_curry)]
