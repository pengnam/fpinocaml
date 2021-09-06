open Fpinocaml_chapter11


module T = struct

  type _ t =
      Return: 'a -> 'a t
    | Suspend:  (unit -> 'a ) -> 'a t
    | FlatMap: 'a t * ('a -> 'b t) -> 'b t

  let flat_map ta ~f = FlatMap(ta, f)

  let map ta ~f =
    flat_map ta ~f:(fun x -> Return (f x))

  (*Honestly it's an API and normally people would use a return here*)
  let unit fa = Suspend (fa)
end

include T

include Monad.Make(T)


let rec run (io: 'a t): 'a =
  match io with
  | Return(a) -> a
  | Suspend(r) -> r()
  | FlatMap(x, f) -> match x with
    | Return(a) -> run (f a)
    | Suspend(x) -> run (f (x()))
    | FlatMap(y, g) -> run (FlatMap (y, (compose ~fa:g ~fb:f)))

let print_line (s:string): unit t =
  Suspend (fun () -> (print_endline s))

let p: unit t = (forever (print_line "printing..."))
