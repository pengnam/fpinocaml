open Monoid
open Core

module ProductMonoid =
  functor (Ta: T) (Tb:T) (Ma: MonoidResult with type t = Ta.t) (Mb: MonoidResult with type t = Tb.t)
    -> struct
      module T0: MonoidType with type t = (Ta.t * Tb.t) = struct
        type t = (Ta.t * Tb.t)
        let op (s1) (s2) = match s1, s2 with
          | (a1, b1), (a2, b2) -> (Ma.op a1 a2, Mb.op b1 b2)

        let zero = (Ma.zero, Mb.zero)
      end
      include MonoidFunctor(T0)
    end

module MapMergeMonoid =
  functor (C:Comparable) (Tv: T)
    (Mv: MonoidResult with type t = Tv.t)
    -> struct

      module T0: MonoidType with type t = (Tv.t C.Map.t) = struct
        type t = (Tv.t C.Map.t)
        let zero = (Map.empty (module C))
        let op s1 s2 =
          let find_or_else m k = Option.value (Map.find m k) ~default:Mv.zero in
          let all_keys = Set.union (Map.key_set s1) (Map.key_set s2)  in
          Set.fold
            ~init:zero
            ~f:(
              fun prev_map ->
              fun key ->
                Map.add_exn
                  prev_map
                  ~key:key
                  ~data:(Mv.op (find_or_else s1 key) (find_or_else s2 key))
            )
            all_keys

      end
      include MonoidFunctor(T0)
    end

module FunctionMonoid =
  functor (Ta:T) (Tb: T) (Mb: MonoidResult with type t = Tb.t)
    -> struct
      module T0: MonoidType with type t = (Ta.t -> Tb.t) = struct
        type t = (Ta.t -> Tb.t)
        let zero = fun _ -> Mb.zero
        let op s1 s2 = fun a -> (Mb.op (s1 a) (s2 a))

      end
      include MonoidFunctor(T0)
    end

(*TODO: in this scenario, does A always have to be a comparable?*)
let bag (aa: int Array.t): ((int, int, Int.comparator_witness) Map.t) =
  Array.fold
    aa
    ~init:(Map.empty (module Int))
    ~f:(fun map -> fun a  ->
        let prev_count = Map.find map a in
        match (prev_count) with
        | None -> Map.add_exn map ~key:a ~data:1
        | Some(c) -> Map.remove map a  |> Map.add_exn ~key:a ~data:(c + 1)
      )
