signature Str =
sig
   eqtype str
   val rep_string : str -> str
   val prt_string : (str -> 'a) -> str -> 'a
   val toMLString : str -> string
   val fromMLString : string -> str
   val deconstruct_str : ('a -> 'b) -> (('a -> string * string * 'b list) -> 'c) -> 'c
   val construct_str : ('a -> str) ->
                       (str -> 'b) -> string * 'a list -> 'b
end

functor AbstractStr
           (eqtype str
            val rep_string : str -> str
            val prt_string : (str -> 'a) -> str -> 'a
            val toMLString : str -> string
            val fromMLString : string -> str) :> Str
  where type str = str =
struct
   type str = str
   fun deconstruct_str str =
      fn f => f (fn s => ("str","string",[str s]))
   val rep_string = rep_string
   val prt_string = prt_string
   val toMLString = toMLString
   val fromMLString = fromMLString
   fun construct_str str rep_str = 
      fn arg => rep_str
           (case arg of ("string",[s]) => rep_string (str s)
                      | (c,l) => raise Fail "str: no case")
end
