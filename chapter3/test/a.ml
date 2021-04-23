(*
   Tests for Chapter2.
   *)
(*
let test_fib () =
    let check_fib msg input expected = Alcotest.(check bool) msg true (Fpinocaml_chapter3.A.fib input = expected) in
    check_fib "fib 1" 1 1;
  check_fib "fib 13" 12 144

let test_sorted () =
    let check_sorted msg input f expected = Alcotest.(check bool) msg expected (Fpinocaml_chapter3.A.is_sorted input f) in
    check_sorted "simple sorted" [1;3;3] (<) true;
  check_sorted "reversed" [1;3;3] (>) false;
  check_sorted "empty" [] (>) true


let test_curry() = Alcotest.(check int ) "simple curry" 3 (Fpinocaml_chapter3.A.curry (fun tup -> (fst tup) + (snd tup)) 1 2 )

let tests = [ ("fib", `Quick, test_fib);("sorted", `Quick, test_sorted) ;("curry", `Quick, test_curry)]
*)

open Fpinocaml_chapter3.A;;

let check msg actual expected = Alcotest.(check (bool)) msg true (actual = expected)


let test_empty() = check "empty list" (MyList.empty) MyList.Nil  ;;

let test_cons() = check "cons list" (MyList.cons 1 MyList.empty)  (MyList.Cons(1, MyList.Nil) ) 


let list1 = (MyList.Cons(1, MyList.Nil))
let list2 = (MyList.Cons(3, MyList.Cons(2, MyList.Nil)))
let final = (MyList.Cons(3, MyList.Cons(2, MyList.Cons(1, MyList.Nil))))

let test_concatenate() = check "concatenate list" (MyList.concatenate list2 list1) final


let tests = [("list empty", `Quick, test_empty);("list cons", `Quick, test_cons);("list concatenate", `Quick, test_concatenate);  ]

