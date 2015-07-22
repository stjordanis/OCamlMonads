type t

type input =
  | Insert_coin
  | Turn

val make : candies:int -> coins:int -> t

val run : input list -> t -> int * int
