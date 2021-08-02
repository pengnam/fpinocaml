
open Core
open Monoid


type stub = string
type wc = Stub of stub| Part of (stub * int * stub)


module WcMonoid =  struct
  module T0: MonoidType with type t = wc = struct
    type t = wc
    let zero = Stub("")
    let op w1 w2 = match w1, w2 with
      | Stub(s), Part(s1, c, s2) -> Part(String.concat[s; s1;], c, s2)
      | Part(s1, c, s2), Stub(s)  -> Part(s1, c,String.concat[s2; s;])
      | Part(s1, c1, s2),Part(s3, c2, s4)  -> if ((String.is_empty s2) && (String.is_empty s3)) then  Part(s1, c1 + c2, s4) else Part(s1, c1+c2 + 1, s4)
      | Stub(s1), Stub(s2) -> Stub(String.concat [s1;s2])
  end

  module T1 = (MonoidFunctor(T0))

  include T1

  let count_words (word:string) =
    let word_array = String.to_array word in
    let convert c =  if (Char.is_whitespace c) then Part("", 0, "") else Stub("") in

    Array.fold_map word_array (module T1) convert


end
