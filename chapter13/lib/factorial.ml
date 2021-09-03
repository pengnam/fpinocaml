open Core
let read_line: string Io.t = {
  run = fun _ ->In_channel.(input_line_exn stdin)
}
 (*Too lazy to do it in a monadic way as not given*)
let factorial (n:int): int Io.t =
  {
    run = (fun _ ->
    let acc = ref 1 in
    Stream.iter (fun a -> acc := !acc * a ) (Stream.from (fun x -> if x < n then Some(x +1) else None));
    !acc
      )
  }


let factorial_repl: unit Io.t = Io.do_while (read_line) ~cond:(
    fun line ->
          if (String.equal line "q")
          then (Io.unit (fun () -> false))
          else (
            Io.flat_map (factorial (int_of_string line))
              ~f:(
                fun r -> {run = fun _-> (let message = Format.sprintf "factorial: %d" r in print_endline message; true)}
              )
          )

      )
