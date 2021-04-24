
open Fpinocaml_chapter3.A

module MyEither = struct
    type ('a, 'b) t = Left of 'a| Right of 'b

    let map f = function 
        | Right x -> Right (f x)
        | Left y -> Left y

    let flat_map f = function 
        | Right x -> f x
        | Left y -> Left y

    let or_else a = function
        | Right x -> Right x
        | Left _ -> a()

   
    let map2 f ea eb =
        match ea, eb with
        | Right a, Right b -> Right (f a b)
        | Left a, _ -> Left a
        | _, Left b -> Left b
    

    let rec traverse f = function
        | MyList.Nil -> Right(MyList.Nil)
        | MyList.Cons(h, t) -> flat_map (fun sh -> map (fun st -> MyList.concatenate sh st)  (traverse f t))(f h) 

    let sequence l = 
        traverse (fun x -> Right(x)) l

end;;


