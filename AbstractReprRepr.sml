signature ReprRepr =
sig
   type repr
   type reprrepr
   val rep_repr : repr -> repr -> reprrepr
   val rep_nil : unit -> reprrepr
   val prt_reprrepr : (unit-> 'a) -> (repr -> repr -> 'a) -> reprrepr -> 'a 
   val deconstruct_reprrepr :
        ((unit -> string * string * 'b list) -> 
         ('a -> 'a -> string * string * 'a list) ->'c) -> 'c
   val construct_reprrepr : (reprrepr -> 'b) -> string * repr list -> 'b
end

functor AbstractReprRepr
  (type reprrepr
   type repr
   val rep_repr : repr -> repr -> reprrepr
   val rep_nil : unit -> reprrepr
   val prt_reprrepr : (unit -> 'a) -> (repr -> repr -> 'a) ->
                        reprrepr -> 'a 
  ) :> ReprRepr
  where type repr = repr
    and type reprrepr = reprrepr =
struct
   type repr = repr
   type reprrepr = reprrepr
   val rep_repr = rep_repr
   val rep_nil = rep_nil
   val prt_reprrepr = prt_reprrepr
   val deconstruct_reprrepr  =
      fn f => f (fn () => ("reprrepr","nil",[]))
                (fn r => fn r' => ("reprrepr","repr",[r, r']))
   fun construct_reprrepr rep_reprrepr =
      fn arg => rep_reprrepr
           (case arg of ("repr",[r,r']) => rep_repr r r'
                      | ("nil",[]) => rep_nil ()
                      | (c,l) => raise Fail "reprrepr: no case")
end
