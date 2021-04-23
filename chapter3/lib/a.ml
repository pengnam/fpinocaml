module MyList = struct
    type 'a t = Nil | Cons of 'a *  'a t
   
    let empty = Nil

    let cons x s = Cons (x, s) 

    let tail = function
        | Cons(_, t) -> t
        | Nil -> failwith "Empty list"

    let set_head x = function
        | Cons(_, t) -> Cons(x, t)
        | Nil -> failwith "Empty list"


    let rec drop n = function
        | Cons(_, t) when n > 1 -> drop (n-1) t
        | _ -> Nil

    let rec fold_left op acc = function
        | Nil   -> acc
        | Cons(h,t) -> fold_left op (op acc h) t

    let rec fold_right op acc = function
        | Nil   -> acc
        | Cons(h,t) -> op (fold_right op acc t) h

    let rec map f = function
        | Nil -> Nil
        | Cons(h, t) -> Cons(f h , map f t)
    
    let rec concatenate a b =
        match a with
        | Nil -> b
        | Cons(h, t) -> Cons(h, concatenate t b)

    let rec flat_map f = function
        | Nil -> Nil
        | Cons(h, t) -> concatenate (f h) (flat_map f t)

    let rec zip_with a b = match a,b with
        | Cons(ha, ta), Cons(hb, tb) -> Cons((ha, hb), (zip_with ta tb))
        | _ -> Nil

    (* To look at again *)
    let rec has_subsequence l sub = match l, sub with
        | Cons(h, t), Cons(hs, ts) -> ((hs = h) && (has_subsequence t ts)) || (has_subsequence t sub)
        | _, Nil -> true
        | Nil, Cons(_, _) -> false



end;;
