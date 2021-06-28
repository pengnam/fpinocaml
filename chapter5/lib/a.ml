open Fpinocaml_chapter3.A

module MyStream = struct
    (* Do I need to make the head lazy as well? *)
    type 'a t = Empty | Cons of ('a) *  (unit->'a t)

    let empty = Empty

    let cons x xs = Cons (x, xs)

    let head_option = function
        | Empty -> None
        | Cons(h, _) -> Some (h)

end;;

    module MyLazyStream = struct
        type 'a t = Empty | Cons of 'a * ('a t Lazy.t)

        let cons x xs = Cons (x, lazy xs)

        let rec to_list = function
            | Empty -> MyList.empty
        | Cons(h, t) -> MyList.cons h (to_list (Lazy.force t))

        let head_option = function 
            | Empty -> None
        | Cons(h, _) -> Some(h)

        let head = function 
            | Empty -> failwith "Yo this is empty" 
        | Cons(h, _) -> h

        let tail = function 
            | Empty -> failwith "Yo this is empty" 
        | Cons(_, t) -> Lazy.force t

        let rec take n = function
            | Cons(h, t) -> if n > 1 then (cons h (take (n-1) (Lazy.force t))) else Empty
        | Empty -> Empty

    (* Lazy take*)
        let take_lazy n = function
            | Cons(h, t) -> if n > 1 then Cons(h,  lazy (take (n-1) (Lazy.force t))) else Empty
        | Empty -> Empty

        let rec drop n = function
            | Cons(_, t) -> if n > 1 then drop (n-1) (Lazy.force t) else Empty 
        | Empty -> Empty 

        let rec map2 f  ls1 ls2 = match (ls1, ls2) with
        | Cons (h1, tf1), Cons (h2, tf2) -> Cons (f h1 h2, lazy ( map2 f (Lazy.force tf1 ) (Lazy.force tf2 )))
        | _ -> Empty

        let sum = map2 (+)

        let rec fold_right op acc = function 
            | Cons(h,t) -> op (fold_right op acc (Lazy.force t)) h
            | Empty -> acc


        let exists p =
            fold_right (fun others h->  (p h) || others) false

        let for_all p = 
            fold_right (fun others h->  (p h) && others) true

        let take_while p = 
            fold_right (fun others h -> if (p h) then (MyList.cons h others) else others) MyList.empty


        let head_option2  (l: 'a t): ('a option)= 
            fold_right (fun _ h -> Some(h)) (None) l


    (*
     This throws an error because it is still possible to pass in a list and not return the right None type
        let head_option2  = 
            fold_right (fun _ h -> Some(h)) (None) 
        *)
        let map f = 
            fold_right (fun others h -> cons (f h) others) Empty  

        let rec map_lazy f = function
            | Empty -> Empty
        | Cons(h, t) -> Cons(f h, lazy (map_lazy f (Lazy.force t)))

        let filter f =
            fold_right (fun others h -> if (f h) then (cons h others) else others) Empty  

        let append b = 
            fold_right (fun others h -> (cons h others)) b

        let flat_map f =
            fold_right (fun others h -> (append (f h) others)) Empty  


    (* Supposedly slightly more efficient since its only one object referencing itself*)
        let constant2 a =
            let rec x = Cons(a, lazy x) in
            x

        let rec from a = 
            Cons(a , lazy (from (a+1)))


        let rec fibs =
            Cons(0, lazy (
                Cons(1, lazy (
                    sum (tail fibs) fibs))));;


        (* Though: should unfold return lazy? *)
        let rec unfold z f = 
            match (f z) with 
            | None -> Empty
            | Some (a, s) -> Cons(a, lazy (unfold s f))

        let fibs_unfold = 
            unfold (0,1) (fun p -> Some(fst p, (snd p, fst p + snd p)))


        let map_unfold f s =
            unfold s (fun x -> match x with
            | Empty -> None
            | Cons(h, t) -> Some( (f h), Lazy.force t))

        let take_unfold n s = 
            unfold (s, n) (fun x -> match x with
            | (Cons(h, t), i) when (i > 0) -> Some(h, (Lazy.force t, (i-1)))
            | _ -> None
            )

        let take_while_unfold f s = 
            unfold s (fun x -> match x with
            | Cons(h, t) when (f h)-> Some( (h, Lazy.force t))
            | _ -> None
            )

        let zip_with_unfold f sa sb = 
            unfold (sa, sb) (fun x -> match x with
            | Cons(ha, ta), Cons(hb, tb) -> Some((f ha hb), (Lazy.force ta, Lazy.force tb))
            | _ -> None
            )

        let zip_all sa sb =
            unfold (sa, sb) (fun x -> match x with
            | Cons(ha, ta), Cons(hb, tb) -> Some((Some ha, Some hb), (Lazy.force ta, Lazy.force tb))
            | Cons(ha, ta), Empty -> Some((Some ha, None), (Lazy.force ta, Empty))
            | Empty, Cons(hb, tb) -> Some((None, Some hb), (Empty, Lazy.force tb))
            | _ -> None
            )

        let starts_with (s: 'a t) (sa: 'a t) =
            for_all (fun x -> match x with
                | Some a, Some b -> a = b
                | _ -> false
            )
            (
                take_while_unfold (
                    fun x -> match x with
                |Some _, Some _ -> true
                |None, Some _-> true
                | _ -> false
                ) (zip_all s sa) 
            )


        let rec find (f: 'a -> bool) (sa: 'a t): 'a Option.t =
          match sa with
          | Cons(ha, ta) -> if (f ha) then Some(ha) else find f (Lazy.force ta)
          | Empty -> None


end;;
