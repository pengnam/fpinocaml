open Fpinocaml_chapter7.A
open Core
module type Basic = sig
  type 'a t
  val unit: (unit->'a) -> 'a t
  val flat_map: 'a t -> f:('a -> 'b t) -> 'b t
end

module type S = sig
  include Basic
  val map: 'a t -> f:('a -> 'b) -> 'b t
  val map2: 'a t-> 'b t  -> f:(('a * 'b) -> 'c) -> 'c t
  val sequence: 'a t List.t -> 'a List.t t
  val traverse: 'a List.t -> ('a -> 'b t) -> 'b List.t t
  val replicate_m: int -> 'a t -> 'a list t
  val filter_m: 'a list -> ('a -> bool t) -> 'a list t
  val compose: fa:('a -> 'b t)-> fb:('b-> 'c t)-> ('a -> 'c t)
  (*effects chapter*)
  val do_while: 'a t -> cond:('a -> bool t) -> unit t
  val forever: 'a t -> 'b t
end

module Make: functor (X:Basic)-> (S with type 'a t := 'a X.t) =
  functor  (X:Basic) -> struct
    include X
    let map ta ~f =
      flat_map ta ~f:(fun a -> unit (fun ()->f a ))

    let map2 ta tb ~f =
      flat_map ta ~f:(fun a-> map tb ~f:(fun b-> (f (a, b))))

    let sequence lma =
      lma
      |> List.fold
        ~f:(
          fun prev -> fun ma ->
            (map2 prev ma ~f:(fun (la, a) -> List.append la [a]))
        )
        ~init:(unit (fun () ->[]))
    let traverse la f =
      la
      |> List.fold
        ~f:(
          fun prev -> fun a ->
            let mb = f a in
            map2 prev mb  ~f:(fun (lb, b) -> List.append lb [b])
        )
        ~init:(unit (fun () ->[]))

    let replicate_m n ma =
      sequence (List.init n ~f:(fun _ -> ma))


    let filter_m la f =
      la
      |> List.fold
        ~f:(
          fun prev -> fun a ->
            let mb = f a in
            (
              map2 prev mb
                ~f:(
                  fun (la, b) ->
                    (if b then la
                     else (List.append la [a]))
                )
            )
        )
        ~init: (unit (fun () ->[]))

    let compose ~fa ~fb =
      fun a -> flat_map (fa a) ~f:fb


    let _flat_map_2 (ma:'a t) (f: 'a -> 'b t): ('b t) =
      compose ~fa:(fun _ -> ma) ~fb:f ()



    let _join (mma: 'a t t): 'a t =
      flat_map mma ~f:(fun a -> a)

    let _flat_map_3 (ma:'a t) (f: 'a -> 'b t): ('b t) =
      _join (map ma ~f:f)

    let _compose_2 (fa:('a -> 'b t)) (fb:('b-> 'c t)): ('a -> 'c t) =
      fun a -> _join (map (fa a) ~f:fb)

    let rec do_while ta ~cond =
      flat_map ta ~f:(
        fun a -> flat_map (cond a) ~f: (
            fun ok ->
              if (ok) then (do_while (ta) ~cond:(cond)) else unit(fun() -> ())
          )
      )


    let rec forever ta =
      flat_map ta ~f:(fun _ -> forever ta)
  end


module ParMonad = Make(
  struct
    type 'a t = 'a Par.t
    let unit fx = Par.lazy_unit fx
    let flat_map ta ~f =
      Par.flat_map ta f
  end
  )


module OptionMonad = Make(
  struct
    type 'a t = 'a option
    let unit fx = Some(fx())
    let flat_map ta ~f =
      match ta with
      | Some(x) -> f x
      | None -> None
  end
  )



module StreamMonad = Make(
  struct
    type 'a t = 'a Stream.t
    let unit fx = Stream.from (fun _-> Some(fx()))

    let rec fold (ta: 'a t) ~(f:'b -> 'a -> 'b) ~(init:'b)=
      try (
        let a = Stream.next ta in
        fold ta ~f:f ~init:(f init a)
      ) with Stream.Failure -> init
    let flat_map ta ~f =
      fold ta
        ~f:(fun prev_stream->fun a -> Stream.iapp prev_stream (f a))
        ~init:(Stream.of_list [])
  end
  )

module ListMonad = Make(

  struct
    type 'a t = 'a List.t
    let unit fx = [fx()]
    let flat_map ta ~f =
      List.fold_left ta ~f:(fun prev -> fun a-> List.append prev (f a )) ~init:[]
  end
  )


(*Exercise 11.9 is interesting*)
(*https://github.com/fpinscala/fpinscala/blob/first-edition/answerkey/monads/09.answer.scala*)


(* This isn't something that can be written simply in Ocaml. *)
(* Type of module requires to flat_map type, and needs to be declared early *)
(* Problem is with type 'a t it needs to just be 'a.. is there an identity type? *)
module IdMonad =
  Make(
  struct
    type 'a t = 'a
    let unit fx = fx()
    let flat_map ta ~f = f ta
  end
  )


(*NOTE:: State monad implementation is seen in Base of Core's Monad implementation*)
module type Basic2 = sig
  type ('a, 'e) t
  val unit: (unit->'a) -> ('a,'e) t
  val flat_map: ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
end

module type S2 = sig
  include Basic2
  val map: ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val map2: ('a, 'e) t-> ('b, 'e) t  -> f:(('a * 'b) -> 'c) -> ('c, 'e) t
  val compose: fa:('a -> ('b, 'e) t)-> fb:('b-> ('c, 'e) t)-> ('a -> ('c, 'e) t)
end

module Make2: functor (X:Basic2)-> (S2 with type ('a, 'e)  t = ('a, 'e) X.t) =
  functor  (X:Basic2) -> struct
    include X

    let map ta ~f =
      flat_map ta ~f:(fun a -> unit (fun ()->f a ))

    let map2 ta tb ~f =
      flat_map ta ~f:(fun a-> map tb ~f:(fun b-> (f (a, b))))

    let compose ~fa ~fb =
      fun a -> (flat_map (fa a) ~f:fb)
  end


(* Q1
 *)

(*Can't I try fitting state into the Make? with the signature above*)

(*Q2
 * Deciding how to create a State. I can try passing a set of primitives, 'a t and run, otherwise I can do it how
 * http://blogs.perl.org/users/cyocum/2012/11/writing-state-monads-in-ocaml.html has done it
*)
(*StateMonad*)

module T = struct
    type ('a, 's) t = ('s -> ('a * 's))
    let bind ta ~f s = (let a, ns = (ta s) in f a ns)
    let map = `Custom (fun ta ~f s -> (let a, ns = (ta s) in (f a, ns)))
    let return a s = (a, s)
end

(*
module type StateMonad = sig
  include Monad.S2
  val get_state: ('a, 's) t -> ('s, 's) t
  val set_state: ('a, 's) t -> 's  -> (unit, 's) t
end
*)
module StateMonad= struct
  include T
  include Monad.Make2(T)

  let get_state _ = fun s ->  (s,s)

  let set_state _ (s:'s)= fun _ -> ((), s)

  let run ta s = ta s

end

(* A little more tricky with the typing but I will ignore for now
module IntStateMonad= struct
  type state = int
  include (StateMonad : StateMonad with type ('a, 's) t := (state -> ('a * state) ) )
end
 *)

(*Zip with index*)

let zip_with_index (la: 'a List.t): (int * 'a) List.t =
  let sm = List.fold_left
    (*'a is list 's is int *)
    ~init:(StateMonad.return [])
    ~f:StateMonad.(
      fun acc a ->
        acc >>= (
          fun xs ->  (get_state acc) >>= (
              fun n -> (set_state acc (n + 1)) >>| (
                  fun _ -> ((n, a) :: xs)
                )
            )
         )
    )
    la in
 List.rev (fst (StateMonad.run sm 0))

