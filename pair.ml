type ('a,'b) t = 'a * 'b
let uncurry2 : ('a -> 'b -> 'c) -> ('a,'b) t -> 'c =
  fun f -> fun (a,b) -> f a b
let uncurry3 : ('a -> 'b -> 'c -> 'd) -> (('a,'b) t,'c) t -> 'd =
  fun f -> fun ((a,b),c) -> f a b c
let uncurry4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ((('a,'b) t,'c) t,'d) t -> 'e =
  fun f -> fun (((a,b),c),d) -> f a b c d