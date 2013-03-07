signature Repr2 =
sig
   type repr
   type str
   type reprrepr
   val rep_str : str -> repr
   val rep_reprrepr : reprrepr -> repr
   val prt_repr : (str -> 'a) -> 
                  (reprrepr -> 'a) ->
                   repr -> 'a
   val deconstruct_repr :
         ('k -> 'l) -> ('m -> 'n) ->
        (('k -> string * string * 'l list) ->
         ('m -> string * string * 'n list) -> 'o) -> 'o
   val construct_repr :
        ('a -> str) ->
        ('a -> reprrepr) -> (repr -> 'b) -> string * 'a list -> 'b
end

functor AbstractRepr
  (type repr
   eqtype str
   type reprrepr
   val rep_str : str -> repr
   val rep_reprrepr : reprrepr -> repr
   val prt_repr : (str -> 'a) -> 
                  (reprrepr -> 'a) ->
                   repr -> 'a
  ) :> Repr2
  where type str = str
    and type repr = repr
    and type reprrepr = reprrepr =
struct
   type str = str
   type repr = repr
   type reprrepr = reprrepr
   val rep_str = rep_str
   val rep_reprrepr = rep_reprrepr
   val prt_repr = prt_repr
   fun deconstruct_repr str reprrepr =
      fn f => f (fn s => ("repr","str", [str s]))
                (fn r => ("repr","reprrepr",[reprrepr r]))
   fun construct_repr str reprrepr rep_repr =
      fn arg => rep_repr
           (case arg of ("str",[s]) => rep_str (str s)
                      | ("reprrepr",[r]) => rep_reprrepr (reprrepr r)
                      | (c,l) => raise Fail "repr: no case")
end
