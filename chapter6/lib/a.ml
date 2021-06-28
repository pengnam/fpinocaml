open Fpinocaml_chapter3.A


module SimpleRng = struct
    type rng = int64;;

    type 'a rand = (rng -> ('a * rng)) ;;

    let next_int seed = 
        (*let new_seed = Int64.logand (Int64.add (Int64.mul seed  0x5DEECE66DL)  0xBL)  0xFFFFFFFFFFFFL in *)
        let new_seed = Int64.add (Int64.mul seed  0x5DEECE66DL)  0xBL in 
        (*let n = Int64.to_int (Int64.shift_right_logical new_seed 16) in*)
        let n = Int64.to_int (new_seed) in
        (n, new_seed)

    let non_negative_int seed = 
        let (next_val, new_seed) = next_int seed in 
        let actual_next_val = if (next_val > 0) then next_val else (-1) * (next_val+1) in 
        actual_next_val, new_seed

    let double seed =
        let (next_val, new_seed) = non_negative_int seed in 
        let actual_next_val = (Float.of_int next_val) /. (Float.of_int Int.max_int) in 
        (actual_next_val, new_seed)


    let ints count seed =
        let rec ints_helper count seed acc =
            if (count > 0) then (let new_int, next_seed = next_int seed in (ints_helper (count - 1) next_seed (MyList.Cons( new_int, acc))))
            else acc, seed 
        in
        ints_helper count seed MyList.empty

    let int_rng: int rand = next_int

    let  unit_rng (a: 'a): 'a rand = fun (rng) -> (a, rng)

    let map (s: 'a rand) (f: 'a -> 'b): 'b rand =
        fun (r: rng) -> let (a, r2) = s r in
        (f a, r2)

    let non_negative_even: int rand = 
        map non_negative_int (fun x -> x - (x mod 2))

    let double2 =
        map non_negative_int (fun next_val -> (Float.of_int next_val) /. (Float.of_int Int.max_int)) 

    let map2 (ra: 'a rand) (rb: 'b rand) (f: ('a * 'b) -> 'c): 'c rand =
        fun (r: rng) -> 
            let (a, r2) = ra r in
                let (b, r3) = rb r2 in
                    (f (a, b), r3)
    let both (ra: 'a rand) (rb: 'b rand): (('a * 'b) rand) =
        map2 ra rb (fun x -> x)

    let sequence (l: ('a rand) MyList.t): ('a MyList.t) rand  = 
        fun seed -> MyList.fold_left 
        (fun ( acc, prev_seed) h -> 
            let (res, next_seed) = h prev_seed in
            (MyList.cons res acc, next_seed)
        ) 
        ( MyList.empty, seed)
        l
    let flat_map (f: 'a rand) (g: 'a -> 'b rand): 'b rand =
        fun (r:rng) -> 
            let (a, r2) = f r in 
                g a r2

    (* There is a better answer in the original scala answer sheet*)
    let non_negative_less_than (n:int): int rand =
        fun (r:rng) ->
            (flat_map non_negative_int (fun i -> (fun rng2 -> (i mod n, rng2)))) r

    let map_2 (s: 'a rand) (f: 'a -> 'b):'b rand =
        fun (r:rng) ->
            flat_map s ( fun a -> unit_rng (f a)) r

    let map2_2 (ra: 'a rand) (rb: 'b rand) (f: ('a * 'b) -> 'c): 'c rand =
        fun (r:rng) -> flat_map ra ( fun a -> map_2 rb (fun b -> f (a, b))) r

    
        
    let boolean:bool rand = 
      fun (r:rng) -> let (x, r2) = non_negative_int r in
        if (x mod 2 == 1) then (true, r2) else (false, r2)


end;;

