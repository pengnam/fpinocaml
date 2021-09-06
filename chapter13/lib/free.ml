(*generalization of async and tailrec (newio)*)

open Core

(*13.1*)
module Make(Free:T1) = struct
    module T = struct


        type _ t =
            Return: 'a -> 'a t
            | Suspend:  ('a Free.t ) -> 'a t
            | FlatMap: 'a t * ('a -> 'b t) -> 'b t

        let bind ta ~f = FlatMap(ta, f)

        let map =
            `Custom (fun ta ~f -> bind ta ~f:(fun x -> Return (f x)))

        let return a = Return (a)
    end


    include T
    include (Monad.Make(T): Monad.S with type 'a t := 'a t)


end

(*Not doing 13.2 cause its too similar*)


(*13.3*)
module MakeMonad(M: Monad.S) = struct

  include (Make(struct type 'a t = 'a M.t end))
  
  let rec run (ta: 'a t): 'a M.t =
    match ta with
    | Return(a) -> M.return(a)
    | Suspend(r) -> r
    | FlatMap(Suspend(x), f) -> M.bind x ~f:(fun a-> run(f a))
    | FlatMap(Return(a), f) ->  run (f a)
    | FlatMap(FlatMap(x, g), f) -> run (x >>= fun a -> (g a) >>= f)

end

(*Page 245*)

module type Translate = sig 
  type 'a f
  type 'a g

  val apply:  'a f -> 'a g

end

(*Here, M can be seen as a destination monad*)
module MakeFreeMonad(G: Monad.S)(Tr: Translate with type 'a g = 'a G.t)  = struct

  include (Make(struct type 'a t = 'a Tr.f end))

  let rec run_free (ta: 'a t): 'a G.t =
    match ta with
    | Return(a) -> G.return(a)
    | Suspend(r) -> (Tr.apply r)
    | FlatMap(Suspend(x), f) -> G.bind (Tr.apply x) ~f:(fun a-> run_free(f a))
    | FlatMap(Return(a), f) ->  run_free (f a)
    | FlatMap(FlatMap(x, g), f) -> run_free (x >>= fun a -> (g a) >>= f)

end
