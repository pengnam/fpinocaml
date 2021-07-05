open Core
module type MonoidType = sig
  type t
  val op: t -> t -> t
  val zero: t
end

module type MonoidResult = sig
  type t
  val op: t -> t -> t
  val zero: t
  (*Others to be included*)
end
module MonoidFunctor(MonoidTypeInstance: MonoidType) = struct
  include MonoidTypeInstance
end



module StringMonoid:MonoidResult = struct
  module T0 = struct
    type t = string
    let op (s1:t) (s2:t) = String.concat [s1;s2]
    let zero = ""
  end

  include MonoidFunctor(T0)

end

module IntAdditionMonoid:MonoidResult = struct
  module T0 = struct
    type t = int
    let op (s1:t) (s2:t) = Int.(s1 + s2)
    let zero = 0
  end
  include MonoidFunctor(T0)
end


module BooleanOrMonoid:MonoidResult = struct
  module T0 = struct
    type t = bool
    let op (s1:t) (s2:t) = (s1 || s2)
    let zero = true
  end
  include MonoidFunctor(T0)
end




(* use of an intermediate dummy module here *)
(* source: http://rgrinberg.com/posts/free-monads-in-the-wild-ocaml/ *)
(* https://www.mseri.me/typeclass-ocaml/ *)
module type GENERIC_TYPE_WORKAROUND = sig type t end

module OptionMonoid (T: GENERIC_TYPE_WORKAROUND)= struct
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
module Dual(MonoidInstance: MonoidResult):MonoidResult = struct
  include MonoidInstance
  let op (s1:t) (s2:t) = MonoidInstance.op s2 s1
end

module FirstOptionMonoid = OptionMonoid
module SecondOptionMonoid(T: GENERIC_TYPE_WORKAROUND) = Dual(FirstOptionMonoid(T))


module EndoMonoid(T: GENERIC_TYPE_WORKAROUND) = struct
  module T0 = struct
    type t = T.t -> T.t
    let op (s1:t) (s2:t) = fun x -> s1 (s2 x)
    let zero = ident
  end

  include MonoidFunctor(T0)
end

include Fpinocaml_chapter8.A
let testMonoid (type a) (module Eq: Comparable.S with type t = a) (module M: MonoidResult with type t = a)  (gen: a Gen.s):  Prop.t=
  let triple = Gen.flat_map gen
      (fun x -> Gen.flat_map gen
          (fun y -> Gen.map gen
              (fun z  -> (x,y,z))
          )
      ) in
  Prop.for_all (triple) (fun (x, y, z) -> Eq.((M.op x (M.op y z)) = (M.op x (M.op y z)) ))
