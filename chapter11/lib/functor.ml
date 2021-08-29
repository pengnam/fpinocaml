open Core
module type Base = sig
  type 'a t
  val map: 'a t -> f:('a -> 'b) -> 'b t
end

module type S = sig
  type 'a t
  val map: 'a t -> f:('a -> 'b) -> 'b t
end
(*Law 1: Mapping over a identity function should itself be an identity*)


module type S2 = sig
  include S
  val distribute: ('a *'b) t -> ('a t * 'b t)
  val codistribute: ((('a t), ('b t)) Either.t) -> (('a, 'b) Either.t) t
end


module Make: functor (X:Base)-> (S with type 'a t = 'a X.t) =
  functor  (X:Base) -> struct
    include X
  end

module Make2: functor (T:S)(X1:Base with type 'a t = 'a T.t) (X2:Base with type 'b t = 'b T.t)-> (S2 with type 'a t = 'a T.t) =
  functor  (T:S)(X1:Base with type 'a t = 'a T.t) (X2:Base with type 'b t = 'b T.t)-> struct
    include T

    let distribute tab =
      (T.map tab ~f:fst, T.map tab ~f:snd )

    let codistribute  e =
      match e with
      | First(fa) -> X1.map fa ~f:(fun x -> First(x))
      | Second(fb) -> X2.map fb ~f:(fun x -> Second(x))

  end
