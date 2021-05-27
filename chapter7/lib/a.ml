
open Base
open Async




module Par = struct
    (* Here, Deferred is Future*)
    type 'a t = unit -> 'a Deferred.t
    (*TODO: See if UnitFuture is meaningful*)

    let unit (a: 'a): 'a t  =  fun () -> return a

    let lazy_unit (a: unit ->'a): 'a t = fun () -> return (a())

    let run (a: 'a t): 'a Deferred.t =  a ()


    let evaluate_deferred (a: 'a Deferred.t): 'a = Thread_safe.block_on_async_exn (
        fun ()-> a >>= fun x -> 
            (* Just to make sure everything prints*)
            Writer.flushed (force Writer.stdout) >>=  (fun _ -> return x)
        )


    let map2 (pa: 'a t) (pb: 'b t) (f: ('a -> 'b -> 'c)): 'c t = 
        fun () ->
        let da = run pa in
        let db = run pb in
        return (f (evaluate_deferred da) (evaluate_deferred db))

    let async_f (f: 'a -> 'b): 'a -> 'b t = fun a -> lazy_unit (fun () -> (f a))

    let map (p: 'a t) (f: 'a -> 'b) = 
        map2(p)(unit ()) (fun a _ -> f a)

    let sort_par(pl:int List.t t): int List.t t = map pl (fun x-> List.sort x ~compare:Int.compare)


    
end;;

module Example = struct
    let sum (ints: int List.t) = 
        let len = List.length ints in
            if (len <= 1) then
                match (List.hd ints) with 
                | None -> 0
                | Some x -> x
            else
                let left, right = List.split_n ints (len / 2) in
                    let sum_list (l: int List.t) = List.sum (module Int) l ~f:(fun x->x) in
                    (sum_list left) + (sum_list right)  


end;;
