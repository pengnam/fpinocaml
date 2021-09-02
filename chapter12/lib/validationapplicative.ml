open Core

module T  = struct
    (*right is success*)
    type ('a, 'e) t =  Success of 'a | Fail of ('e * 'e List.t)

    let return a = Success a

    let _map2 ta tb f = match ta, tb with
      | Success a, Success b -> Success (f a b)
      | Success _, Fail (h, t) -> Fail (h, t)
      |  Fail (h, t), Success _ -> Fail (h, t)
      |  Fail (ha, ta), Fail (hb, tb) -> Fail (ha, List.concat [ta; hb::tb])

    let apply tfa ta = _map2 tfa ta (fun fa a -> fa a)

    let map = `Define_using_apply

end

include Applicative.Make2(T)
