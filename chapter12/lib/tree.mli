
(*
open Core



type 'a tree =
  | Leaf
  | Node of 'a node

and 'a node = {
  value: 'a;
  left:  'a tree;
  right: 'a tree
}
type 'a t = 'a tree

include Container.S1 with type 'a t := 'a t
*)
