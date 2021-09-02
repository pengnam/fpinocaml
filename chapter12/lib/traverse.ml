open Core

module IdApplicative = struct
  module T = struct
    type 'a t = 'a
    let unit fa = fa()
    let map2 a b ~f = f a b
  end
  include T
  include Applicatives.Make(T)
    end


module Traverse (C:Container.S1)(F:Applicatives.S with type 'a t = 'a C.t) (G:Applicatives.S) = struct


  module type Basic = sig
    val defined_tors: [`Traverse of 'a F.t -> ('a -> 'b G.t) -> 'b F.t G.t | `Sequence of 'a G.t F.t -> 'a F.t G.t]
  end

  module type S = sig
    val traverse: 'a F.t -> ('a -> 'b G.t) -> 'b F.t G.t
    val sequence: 'a G.t F.t -> 'a F.t G.t
    (**val map: 'a F.t -> ('a -> 'b) -> 'b F.t*)
  end

  module Make(X:Basic): S = struct
    let traverse fa f = match X.defined_tors with
      | `Traverse tf -> tf fa f
      | `Sequence sf -> sf (F.map ~f:f fa)

    let sequence fga = match X.defined_tors with
      | `Traverse tf -> tf fga (fun x->x)
      | `Sequence sf -> sf fga
  end
end


module TraverseList (F:Applicatives.S with type 'a t = 'a List.t)(G:Applicatives.S) = struct
  include Traverse (List) (F) (G)
  module T = struct
    let defined_tors = `Traverse (
        fun fa f ->
          List.fold
            ~init:(G.unit(fun () -> []))
            ~f:(fun prev a -> G.map2 prev (f a) ~f:(fun pl b -> b::pl))
            fa
      )
  end
  include T
  include Make(T)

end

module TraverseOption (F:Applicatives.S with type 'a t = 'a Option.t)(G:Applicatives.S) = struct
  include Traverse (Option) (F) (G)
  module T = struct
    let defined_tors = `Traverse (
        fun fa f ->
          match fa with
          | None -> G.unit (fun () -> None)
          | Some a -> G.map (f a) ~f:(fun x -> Some x)
      )
    end
  include T
  include Make(T)

end

module TraverseTree(F:Applicatives.S with type 'a t = 'a Option.t)(G:Applicatives.S) = struct
  include Traverse (Option) (F) (G)
  module T = struct
    let defined_tors = `Traverse (
        fun fa f ->
          match fa with
          | None -> G.unit (fun () -> None)
          | Some a -> G.map (f a) ~f:(fun x -> Some x)
      )
    end
  include T
  include Make(T)
end


