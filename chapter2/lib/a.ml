let fib n = 
    let rec fib_tr p pp i = 
        if (i = 1) then pp
        else fib_tr pp (p + pp) (i-1) in
    fib_tr 0 1 n;;


let is_sorted l f =
    let rec is_sorted_helper l f prev = 
        match l, prev with
        (h::t), None -> is_sorted_helper t f (Some h)
        | (h::t), Some v -> (f v h) && is_sorted_helper t f (Some v)
        | [], _ -> true in
    is_sorted_helper l f None;;

let curry f =
    fun a -> (fun b -> f (a,b));;

let uncurry f = 
    fun tup -> (f fst tup) (snd tup);;


let compose f g = 
    fun x -> f (g x);;

