signature BoundVariables =
sig
    type repr
    eqtype str
    val boundvars : str list -> repr -> str list
end

functor BoundVariables
  (type repr
   eqtype str
   structure Induction : Induction
       where type repr = repr and type str = str
   structure Deconstructor : Deconstructor
       where type repr = repr) :> BoundVariables
   where type repr = repr and type str = str =
struct
   type repr = repr
   type str = str
   local
      type boundstate = str list
      type boundstatefns = boundstate -> str -> boundstate
      val boundVarsState:boundstate = []
      val boundVarsFns:boundstatefns =
             (fn bound => fn v => v::bound)
      fun toBoundVars str deconstructor =
         fn output:boundstatefns => fn state => fn (t,c,l) => 
              let val corecur = deconstructor output
                  val addbound = output
                  fun iter state [] = state
                    | iter state (arg::args) =
                         iter (corecur state arg) args
              in case (t,c,l) of ("pat","bindvar", [v,_]) => addbound state (str v) 
                               | ("pat","alias", [v,p]) => corecur (addbound state (str v)) p
                               | ("bind","lambda", [v,e]) => corecur (addbound state (str v)) e
                               | ("atype","forall", [v,t]) => corecur (addbound state (str v)) t
                               | ("pat","pair",l) => iter state l
                               | _ => state
              end
      open Induction
   in
      fun boundvars (state:boundstate) =
         induction (fromRep Deconstructor.deconstr)
               (toBoundVars (dec_repr_str Repr.prt_repr))
                boundVarsFns state
   end
end
