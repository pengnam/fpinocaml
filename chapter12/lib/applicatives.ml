open Core
module type Basic = sig
  type 'a t
  val unit: (unit->'a) -> 'a t
  val map2: 'a t -> 'b t -> f:('a  -> 'b  -> 'c) -> 'c t
end

module type S = sig
  include Basic
  val map: 'a t -> f:('a -> 'b ) -> 'b t
  val traverse: 'a List.t -> f:('a -> 'b t) -> 'b List.t t
  val sequence: 'a t List.t -> 'a List.t t
end


module Make: functor (X:Basic)-> (S with type 'a t := 'a X.t) =
  functor  (X:Basic) -> struct
    include X

    let map ta ~f = map2 (unit (fun ()->()) ) ta ~f:(fun _ -> fun a -> f a)

    let traverse la ~f = List.fold_right
        ~f:(fun a acct -> map2 acct (f a) ~f: (fun acc b -> b::acc))
        ~init:(unit (fun () ->[])) la

    let sequence fas = List.fold_right ~f:(fun ta acct -> (map2 ta acct ~f:(fun a acc -> a::acc))) ~init:(unit (fun () -> [])) fas

    let _traverse la ~f = sequence (List.map ~f:f la)


  end

(*TODO: problem if I include it above is this: https://stackoverflow.com/questions/51254916/the-type-constructor-would-escape-its-scope-when-using-first-class-modules*)
module AddSequenceMap(KM:Comparable) (M:S)= struct
  include M

  let sequence_map ofa = Map.fold
      ~init:(unit (fun () -> Map.empty (module KM)))
      ~f:(fun ~key ~data prev -> (map2 prev data ~f:(fun prev_map v -> (Map.add_exn prev_map ~key:key ~data:v))))
      ofa
end

module type Basic2 = sig
  type 'a t
  val unit: (unit->'a) -> 'a t
  val apply: ('a ->'b) t -> 'a t -> 'b t
end

(*too lazy to implement all*)
module type S2 = sig
  include Basic
  val map: 'a t -> f:('a -> 'b ) -> 'b t
  val map2: 'a t -> 'b t -> f:('a  -> 'b  -> 'c) -> 'c t
  val map3: 'a t -> 'b t -> 'c t->f:('a  -> 'b  -> 'c -> 'd) -> 'd t
end

module Make2: functor (X:Basic2)-> (S2 with type 'a t := 'a X.t) =
  functor  (X:Basic2) -> struct
    include X

    let map ta ~f = apply (unit (fun() -> f)) ta
    let map2 ta tb ~f = apply (map ta ~f:(fun a -> f a)) tb
    let _apply (tf:('a ->'b) t) (ta: 'a t) :'b t =
      map2 ta tf ~f:(fun a f -> f a)

    (*I'm directly using the uncurried version*)
    let map3 ta tb tc ~f =
      let tf = unit( (fun() -> f) ) in
      apply (apply (apply tf ta) tb) tc

  end

module Compose (X:S) (Y:S): S with type 'a t = 'a X.t Y.t = struct

  type 'a t = 'a X.t Y.t

  include Make(struct
      type 'a t = 'a X.t Y.t
      let unit fa = Y.unit (fun() -> (X.unit fa))
      let map2 tma tmb ~f = Y.map2 tma tmb ~f:(fun ma mb -> X.map2 ma mb ~f:f)
    end)
end
