signature UTF8 =
sig

  exception BadUTF8 of string
  val getFirstChar : substring -> ((substring * int) * substring) option
  val getChars : int -> string -> (substring * int) list * substring
  val index : string -> int list
  val getChar : string -> ((string * int) * string) option
  val explode : string -> string list
  val lastChar : string -> (string * int) option
  val size : string -> int
  val chr : int -> string (* May raise Chr *)

  val translate : (string -> string) -> string -> string
end
