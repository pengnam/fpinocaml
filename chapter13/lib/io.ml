open Fpinocaml_chapter11

type 'a io = {
  run: unit -> 'a
}

module T = struct
  type 'a t = 'a io
  let unit fa = {run = fun () -> fa () }
  let map ta f = {
    run = fun () -> f (ta.run ())
  }
  let flat_map ta ~f = {
    run = fun() -> (f (ta.run ())).run()
  }
end

include T

include Monad.Make(T)
