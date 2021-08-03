open Core
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
    val to_list: 'a t -> 'a list
end


module Make: functor (X:Base)-> (S with type 'a t = 'a X.t) =
  functor  (X:Base) -> struct
    include X
    let fold_right (l: 'a t) ~(f:'a -> 'b -> 'b) ~(init:'b)
      =  (
        (*'b for fold_left is 'b -> 'b*)
        X.fold_left l
          ~init:(fun x-> x )
          ~f:( fun prev a -> (fun b -> prev (f a b) ))
      ) init

    let fold_map (type b) l (module M:MonoidResult with type t = b ) ~f =
      X.fold_left l ~init:M.zero ~f:(fun b-> fun a-> M.op b (f a))
    let to_list l =
      List.rev (
        X.fold_left l
          ~init:[]
          ~f: (fun b -> fun a -> a::b)
    )
  end


module FoldableList = Make(
  struct
    type 'a t = 'a list
    let rec fold_left l ~init ~f  =  match l with
      | [] -> init
      | h::t -> fold_left t ~init:(f init h) ~f:f
  end
  )

module FoldableArray= Make(
  struct
    type 'a t = 'a array
    let fold_left l ~init ~f  =
      let len = Array.length l in
      let rec fold_left_helper i prev=
        if i = len then init
        else let h = Array.get l i in
          (fold_left_helper (i+1) (f prev h ) )in
      fold_left_helper 0 init
  end
  )

type 'a tree =
  | Leaf  of 'a
  | Branch of 'a * 'a tree * 'a tree


module FoldableTree = Make(
  struct
    type 'a t = 'a tree
    let rec fold_left t ~init ~f =
      match t with
      | Leaf(a) -> f init a
      | Branch(a,l,r)->
        let prev = fold_left l ~init:init ~f:f in
        let result = f prev a in
        fold_left r ~init:result ~f:f

  end

  )

module FoldOption = Make(
  struct
    type 'a t = 'a option
    let fold_left o ~init ~f =
      match o with
      | None -> init
      | Some(a) -> f init a
  end
  )


