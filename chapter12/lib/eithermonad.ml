open Core

module T  = struct
    (*right is success*)
    type ('s, 'f) t = Left of 'f | Right of 's
    let bind ta ~f  = match ta with
        | Left y -> Left y
        | Right r -> f r
    let map = `Define_using_bind
    let return a = Right a
end

include Monad.Make2(T)
