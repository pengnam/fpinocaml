(*NOTE: JS has a QuickCheck library in Core. Take a look at that.*)
open Core;;
include Fpinocaml_chapter6.A;;
include Fpinocaml_chapter6.B;;
include Fpinocaml_chapter5.A;;


module Gen = struct
  (* Gen as a type of Rng. Note that s is used here instead of t because of a bad decision made TODO *)
  include Make(struct type t = SimpleRng.rng end)

  let choose (start:int) (stop_exclusive:int): int s =
    SimpleRng.(map (non_negative_less_than (stop_exclusive - start)) (fun r -> r + start))

  let unit (a: unit->'a): 'a s =
    fun rng -> (a(), rng)

  let boolean: bool s =
    SimpleRng.boolean

  let list_of_n (n:int) (g: 'a s): 'a List.t s=
    sequence (List.init n ~f:(fun _-> g))
  (*Flat map already defined in state module*)
  let list_of_n_2 (sn:int s) (sa: 'a s): 'a List.t s=
    flat_map sn (fun n -> list_of_n n sa )

  let union (g1: 'a s) (g2: 'a s): 'a s =
    flat_map boolean (fun b -> if b then g1 else g2)
  let weighted (gd1: ('a s * float)) (gd2: ('a s * float)): 'a s=
    let g1, w1 = gd1 and g2, w2 = gd2 in
    let total = w1 +. w2 in
    let thresh = w1 /. total in
    SimpleRng.(fun (r:rng) -> let d, r2 = double r in
                if Float.(d < thresh) then g1 r2 else g2 r2
              )

  (*TODO: Worry*)
  let unsized (g: 'a s)=
    fun (_:int) -> g

end;;

module SGen = struct
  type 'a t = int -> 'a Gen.s


  let apply (sg: 'a t) (n:int): 'a Gen.s =
    (sg n)

  let map (sg: 'a t) (f: 'a -> 'b) =
    fun (n:int) ->
    Gen.map f (sg n)
  let flat_map (sg: 'a t) (f: 'a -> 'b t): 'b t =
    fun (n:int)  -> Gen.flat_map (sg n) (fun a -> ((f a) n))




end;;

module Prop = struct
  type failed_case = string
  type success_count = int
  type test_cases = int
  type max_size = int
  type result = Passed | Falsified of (failed_case * success_count)
  type t = (test_cases * SimpleRng.rng) -> result
  type new_t = (max_size * test_cases * SimpleRng.rng) -> result

  let is_falsified = function
    | Passed -> false
    | Falsified(_)-> true

  let random_stream (g: 'a Gen.s ) (rng: SimpleRng.rng): 'a MyLazyStream.t =
    MyLazyStream.unfold (rng) (fun r -> Some(g r) )

  let for_all (g: 'a Gen.s) (f: 'a -> bool) : t =
    fun (n, rng) -> MyLazyStream.(

        let result_stream = take_lazy n (random_stream g rng) in
        let processed_stream = map_unfold
            (fun (a, i) -> if (f a) then Passed else Falsified("to replace a with something that has a tostring method", i))
            (zip_with_unfold (fun x y -> (x,y)) result_stream (from 0)) in
        match (find is_falsified processed_stream) with
        | None -> Passed
        | Some(res) -> res
      )

    let (&&) (pa: t) (pb: t): t =
      fun rng -> let r = pa rng  in
        if (is_falsified r) then r else (pb rng)



    (*TODO: Consider prepending the message infront of the second argument*)
    let (||) (pa: t) (pb: t): t =
      fun rng -> let r = pa rng  in
        if (is_falsified r) then (pb rng) else (r)




end;;
