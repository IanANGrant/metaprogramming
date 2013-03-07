signature Semantics =
sig
   type repr
   type state
   val map_name : string
   val initstate : state
   val deconstr : state -> repr -> (string * string * repr list) * state
   val reconstr : state -> string * string * repr list -> repr * state
end
