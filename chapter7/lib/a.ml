
open Base
open Async




(* Already non-blocking uwu*)
module Par = struct
  (* Here, Deferred is Future*)
  type 'a t = unit -> 'a Deferred.t

  let unit (a: 'a): 'a t  =  fun () -> return a

  let fork (fa:unit -> 'a t): 'a t =
    fun () -> (schedule' (fa()))

  let lazy_unit (a: unit ->'a): 'a t = fork(fun () -> unit(a()))

  let run (a: 'a t): 'a Deferred.t =  a ()



    (*
     * This is the BLOCKing implementation for a blocking get
    let evaluate_deferred (a: 'a Deferred.t): 'a = Thread_safe.block_on_async_exn (
        fun ()-> a >>= fun x -> 
            (* Just to make sure everything prints*)
            Writer.flushed (force Writer.stdout) >>=  (fun _ -> return x)
        )
    *)


  let map2 (pa: 'a t) (pb: 'b t) (f: ('a -> 'b -> 'c)): 'c t =
    fun () ->
    let da = run pa in
    let db = run pb in
    (Deferred.both da db >>| fun p -> f (fst p) (snd p)  )

  let async_f (f: 'a -> 'b): 'a -> 'b t = fun a -> lazy_unit (fun () -> (f a))

  let map (p: 'a t) (f: 'a -> 'b) =
    map2(p)(unit ()) (fun a _ -> f a)

  let sort_par(pl:int List.t t): int List.t t = map pl (fun x-> List.sort x ~compare:Int.compare)

  let sequence_simple (ps: 'a t List.t): 'a List.t t =
    List.fold_right ~init:(unit []) ~f:(fun ph pacc -> (map2 ph pacc (fun h acc -> h::acc))) ps

  (* Slight difference: this is 'tail-recusive' since unlike simple no stack*)
  let rec sequence_right (ps: 'a t List.t): 'a List.t t=
    match ps with
    | [] -> unit []
    | h::t -> map2 h (sequence_right t) (fun h t -> h::t)

  let sequence_balanced (ps: 'a t Array.t): 'a Array.t t =
    (* concat operator not efficient but will make do for now*)
    let rec sequence_balanced_helper (s:int) (e:int): 'a Array.t t =
      if (s = e) then unit ([||])
      else if ((e - s) = 1) then
        map (Array.get ps s) (fun x -> List.to_array [x])
      else
        let m = (s + 2) / 2 in
        map2 (sequence_balanced_helper s m) (sequence_balanced_helper m e) (fun left right -> Array.append left right)


    in sequence_balanced_helper 0 (Array.length ps)

  let sequence (ps: 'a t List.t): 'a List.t t=
    map (sequence_balanced (List.to_array ps)) (Array.to_list)


  let par_map (ps: 'a List.t) (f: 'a -> 'b): 'b List.t t =
    sequence (List.map ps ~f:(async_f f))



  let par_filter (al: 'a List.t) (f: 'a -> bool): 'a List.t t =
    (* Not parallel:
       lazy_unit(fun ()-> List.filter al ~f:f)
    *)
    let list_of_lists: 'a List.t List.t t = par_map al (fun a -> if (f a) then [a] else []) in
    map list_of_lists List.concat



  let choice (cond: bool t) (t: 'a t) (f: 'a t): 'a t =
    fun() -> cond() >>= fun c -> if c then t() else f()


  let choice_n (n: int t) (choices: 'a t List.t): 'a t =
    fun () -> n() >>= (fun index-> let selected = List.nth_exn choices index in selected())

  let choice_2(cond: bool t) (t: 'a t) (f: 'a t): 'a t =
    choice_n (fun () -> cond() >>|  Bool.to_int ) ([f;t])

  (* `chooser` is actually flatmap *)
  let flat_map(pa: 'a t) (f: 'a-> 'b t): 'b t =
    fun() -> pa() >>= fun a -> (f a) ()

  let choice_flat (cond: bool t) (t: 'a t) (f: 'a t): 'a t =
    flat_map cond (fun c -> if c then t else f)

  let choice_n_flat (n: int t) (choices: 'a t List.t): 'a t =
    flat_map n (fun i -> List.nth_exn choices i)

  let join (ppa: 'a t t): 'a t =
    fun () -> (schedule' (ppa)) >>= fun pa -> (schedule' pa)

  let join_fm (ppa: 'a t t): 'a t =
    flat_map (ppa) (fun pa -> pa)

  let flat_map_join(pa: 'a t) (f: 'a-> 'b t): 'b t =
    join (map pa f)

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
