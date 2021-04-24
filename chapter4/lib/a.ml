module MyOption = struct
    type 'a t = None| Some of 'a 

    let map f = function 
        | Some x -> Some (f x)
        | None -> None

    let flat_map f = function 
        | Some x -> f x
        | None -> None

    let get_or_else a = function
        | Some x -> x
        | None -> a

    let or_else a = function
        | Some x -> Some x
        | None -> a()

    let filter f = function
        | Some x -> if (f x) then Some x else None
        | None -> None
   



end;;
