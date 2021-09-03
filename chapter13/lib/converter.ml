open Core
let read_line: string Io.t = {
  run = fun _ ->In_channel.(input_line_exn stdin)
}

let print_line (message:string): unit Io.t = {
  run = fun _ -> (print_endline message)
}

let farenheit_to_degrees f = (f - 32 ) * 5 / 9

let converter: unit Io.t =
  Io.flat_map
    (print_line "enter a temperature")
    ~f: (
      fun _ ->
        Io.flat_map (Io.map read_line ~f:int_of_string)
          ~f:(fun x -> print_line (Int.to_string (farenheit_to_degrees x)))

    )

