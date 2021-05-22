open Fpinocaml_chapter3.A

module type State  = 
    sig
        type t
    end

module Make(S:State) = struct

    type t = S.t

    type 'a  s = (t -> ('a * t)) ;;

    let unit_state (a: 'a): 'a s= fun (rng) -> (a, rng)

    let sequence (l: ('a s) MyList.t): ('a MyList.t) s  = 
        fun seed -> MyList.fold_left 
        (fun ( acc, prev_seed) h -> 
            let (res, next_seed) = h prev_seed in
            (MyList.cons res acc, next_seed)
        ) 
        ( MyList.empty, seed)
        l
    let flat_map (f: 'a s) (g: 'a -> 'b s): 'b s =
        fun (r:t) -> 
            let (a, r2) = f r in 
                g a r2
    let map (s: 'a s) (f: 'a -> 'b):'b s =
        fun (r:t) ->
            flat_map s ( fun a -> unit_state (f a)) r

    let map2 (ra: 'a s) (rb: 'b s) (f: ('a * 'b) -> 'c): 'c s =
        fun (r:t) -> flat_map ra ( fun a -> map rb (fun b -> f (a, b))) r

    let get: t s = 
        fun (r:t) -> (r,r)

    let set  (s: 'a): unit s = fun (_: t) -> ((), s)

end;;

