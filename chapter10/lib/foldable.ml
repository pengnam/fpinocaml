open Monoid

(*NOTE: Syntax inspired from Core's Monad*)

module type Base = sig
  type 'a t
  val fold_left : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module type S = sig
  type 'a t
  val fold_right: 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b
  val fold_left : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val fold_map: 'a t ->  (module MonoidResult with type t = 'b) -> f:('a -> 'b) -> 'b
end


module Make: functor (X:Base)-> S =
  functor  (X:Base) -> struct
    include X


    let fold_right (l: 'a t) ~(f:'a -> 'b -> 'b) ~(init:'b)
    (*'b for fold_left is 'b -> 'b*)
(* init: 'b -> 'b *)
(*f: ('b -> 'b) -> 'a -> ('b ->'b)*)
      =  (
        X.fold_left l
          ~init:(fun x-> x )
          ~f:( fun prev a -> (fun b -> prev (f a b) ))
      ) init

    let fold_map (type b) l (module M:MonoidResult with type t = b ) ~f =
      X.fold_left l ~init:M.zero ~f:(fun b-> fun a-> M.op b (f a))



end
