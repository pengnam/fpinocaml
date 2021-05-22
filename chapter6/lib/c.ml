
include Fpinocaml_chapter3.A


module CoinMachine = struct
    type candies = int
    type coin = int
    type cm = {locked: bool; candies: int; coins: int}
    type input = Coin | Knob

    include B.Make( struct type t = cm end)

    let empty = {locked=true; candies=10; coins=0}

    let transition (s:cm) (i:input):cm = 
        let  {locked=locked; candies=candies; coins=coins}  = s in 
            if candies <= 0 then
                s
            else
                match locked, i with
                | true, Coin  -> {locked=false; candies=candies; coins=coins+1}
                | false, Knob-> {locked=true; candies=candies-1; coins=coins}
                | true, Knob -> s 
                | false, Coin-> s


    let simulate_machine(inputs: input MyList.t): (int * int) s =
        fun (start:cm) ->
        let  state = MyList.fold_left (fun acc h -> transition acc h) start inputs  in 
            let {locked=_; candies=candies; coins=coins}  = state in 
                (candies, coins), state
        

end;;
