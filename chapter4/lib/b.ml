open Fpinocaml_chapter3.A
open A

let rec sequence = function
    | MyList.Nil -> MyOption.Some(MyList.Nil)
    | MyList.Cons(h, t) -> MyOption.flat_map (fun sh -> MyOption.map (fun st -> MyList.concatenate sh st) (sequence t)) h


let rec traverse f = function
    | MyList.Nil -> MyOption.Some(MyList.Nil)
    | MyList.Cons(h, t) -> MyOption.flat_map (fun sh -> MyOption.map (fun st -> MyList.concatenate sh st)  (traverse f t))(f h) 

let sequence2 l = 
    traverse (fun x -> MyOption.Some(x)) l
