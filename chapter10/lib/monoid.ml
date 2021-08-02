open Core
include Fpinocaml_chapter7.A

(* use of an intermediate dummy module here (look at OptionMonoid)*)
(* source: http://rgrinberg.com/posts/free-monads-in-the-wild-ocaml/ *)
(* https://www.mseri.me/typeclass-ocaml/ *)
module type GENERIC_TYPE_WORKAROUND = sig type t end


module type MonoidType = sig
  type t
  val op: t -> t -> t
  val zero: t
end

module type MonoidResult = sig
  type t
  val op: t -> t -> t
  val zero: t
  (*Exercise 10.6 onwards*)
  val fold_map:  'a list ->('a -> t) -> t
end

module rec MonoidFunctor: functor (MonoidTypeInstance: MonoidType) ->MonoidResult with type t = MonoidTypeInstance.t =
  functor(MonoidTypeInstance: MonoidType) -> struct
    include MonoidTypeInstance

    let rec fold_map al f =
      match al with
      | h :: t -> op (f h)  (fold_map t f)
      | [] -> zero

    let _fold_map_2 al f =
      List.fold_left ~init:zero ~f:(fun c x -> op c (f x)) al
    let _fold_map_3 al f =
      List.fold_right ~init:zero ~f:(fun x c -> op (f x) c) al

    (*    let _fold_left (type a) al init (f:'a -> a ->a) =
          let module EM = EndoMonoid(struct type t = a end )  in
          (EM.fold_map al f) init
          (*PROBLEM: Recurisve functor*)
    *)

  end
and StringMonoid:MonoidResult = struct
  module T0 = struct
    type t = string
    let op (s1:t) (s2:t) = String.concat [s1;s2]
    let zero = ""
  end

  include MonoidFunctor(T0)

end
and IntAdditionMonoid:MonoidResult = struct
  module T0 = struct
    type t = int
    let op (s1:t) (s2:t) = Int.(s1 + s2)
    let zero = 0
  end
  include MonoidFunctor(T0)
end


and BooleanOrMonoid:MonoidResult = struct
  module T0 = struct
    type t = bool
    let op (s1:t) (s2:t) = (s1 || s2)
    let zero = true
  end
  include MonoidFunctor(T0)
end
and OptionMonoid: functor (T: GENERIC_TYPE_WORKAROUND) -> MonoidResult with type t = T.t option =
  functor (T: GENERIC_TYPE_WORKAROUND) ->struct
    module T0 = struct
      type t = T.t option
      let op (s1: t) (s2: t): t = Option.first_some s1 s2
      let zero = None
    end
    include MonoidFunctor(T0)
  end

(* FROM fpinscala answer key:
   // Notice that we have a choice in how we implement `op`.
   // We can compose the options in either order. Both of those implementations
   // satisfy the monoid laws, but they are not equivalent.
   // This is true in general--that is, every monoid has a _dual_ where the
   // `op` combines things in the opposite order. Monoids like `booleanOr` and
   // `intAddition` are equivalent to their duals because their `op` is commutative
   // as well as associative.
   // Notice that we have a choice in how we implement `op`.
   // We can compose the options in either order. Both of those implementations
   // satisfy the monoid laws, but they are not equivalent.
   // This is true in general--that is, every monoid has a _dual_ where the
   // `op` combines things in the opposite order. Monoids like `booleanOr` and
   // `intAddition` are equivalent to their duals because their `op` is commutative
   // as well as associative.
*)
and Dual: functor (MonoidInstance: MonoidResult)->MonoidResult =
  functor (MonoidInstance: MonoidResult)->struct
    include MonoidInstance
    let op (s1:t) (s2:t) = MonoidInstance.op s2 s1
  end

and FirstOptionMonoid: functor(T:GENERIC_TYPE_WORKAROUND) -> MonoidResult = OptionMonoid
and SecondOptionMonoid: functor (T: GENERIC_TYPE_WORKAROUND) -> MonoidResult =
  functor (T: GENERIC_TYPE_WORKAROUND) -> Dual(FirstOptionMonoid(T))


and EndoMonoid: functor (T: GENERIC_TYPE_WORKAROUND) -> MonoidResult with type t = T.t->T.t =
  functor (T: GENERIC_TYPE_WORKAROUND) ->struct
    module T0: MonoidType with type t = T.t->T.t = struct
      type t = T.t -> T.t
      let op (s1) (s2) = fun x -> s1 (s2 x)
      let zero = ident
    end

    include MonoidFunctor(T0)
  end


include Fpinocaml_chapter8.A
let test_monoid (type a) (module Eq: Comparable.S with type t = a) (module M: MonoidResult with type t = a)  (gen: a Gen.s):  Prop.t=
  let triple = Gen.flat_map gen
      (fun x -> Gen.flat_map gen
          (fun y -> Gen.map gen
              (fun z  -> (x,y,z))
          )
      ) in
  Prop.for_all (triple) (fun (x, y, z) -> Eq.((M.op x (M.op y z)) = (M.op x (M.op y z)) ))

module Array = struct
  include Core.Array
  let fold_map (aa:'a t) (module M:MonoidResult with type t = 'b) (f: 'a -> 'b):'b =
    let rec fold_map_helper (s:int) (e:int) =
      let l = e - s in
      if (l <= 0) then
        M.zero
      else if (l =1) then
        f (get aa 0 )
      else
        let mid = (e - s) / 2 in
        (M.op (fold_map_helper s mid) (fold_map_helper (mid + 1) e))
    in

    fold_map_helper 0 (length aa)
end

(* This is the equivalent for Par *)


(*TODO: Fix, and try to understand the include example*)
(* Am I supposed to be exposing the type by defining a MonoidParResult type? *)
module MonoidParFunctor: functor (T: GENERIC_TYPE_WORKAROUND) (MonoidResultInstance: MonoidResult with type t = T.t) -> MonoidResult with type t = MonoidResultInstance.t Par.t=
  functor (T: GENERIC_TYPE_WORKAROUND) (MonoidResultInstance: MonoidResult with type t = T.t) ->struct
    module T0: MonoidType with type t = T.t Par.t = struct
      type t = MonoidResultInstance.t Par.t
      let op (s1) (s2) = Par.map2 s1 s2 MonoidResultInstance.op
      let zero = Par.unit MonoidResultInstance.zero
    end

    module T1: MonoidResult  with type t = T.t Par.t = struct
      include MonoidFunctor(T0)
    end
    include T1
      (*this is par_fold_map*)
        (*Hmm how should I fix this TODO*)
        (*
    let fold_map al f =
      Array.fold_map (T1.t) al (module T1) f
         *)
  end
(*
let par (type t) (module M: MonoidResult with type t = t): (module MonoidResult with type t = t Par.t)  =
  let module T0: MonoidType  = struct
      type t = M.t Par.t
      let op (s1) (s2) = Par.map2 s1 s2 M.op
      let zero = Par.unit M.zero
    end in
  (module struct include MonoidFunctor(T0)  end)
*)

module IntArray = struct
  include Array

  type result = Result of (int * int * bool) | Empty
  let sorted al =
    let module T0: MonoidType with type t = result = struct
        type t = result
        let op (s1) (s2) = match s1, s2 with
          | Empty, _-> s2
          | _, Empty -> s1
          | Result(min_a, max_a, b_a), Result(min_b, max_b, b_b) -> Result(min_a, max_b, b_a && b_b && max_a <= min_b)
        let zero = Empty
      end in
    let module T1 = MonoidFunctor(T0) in
    T1.fold_map  al (fun x-> Result(x, x, true))
end
