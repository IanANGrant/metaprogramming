signature Variables =
sig
    type repr
    eqtype str
    val vars : repr -> str list
end

functor Variables
  (type repr
   eqtype str
   structure Induction : Induction
       where type repr = repr and type str = str
   structure Deconstructor : Deconstructor
       where type repr = repr) :> Variables
   where type repr = repr and type str = str =
struct
   type repr = repr
   type str = str
   local
      fun toVars str deconstructor =
         fn output => fn state => fn (t,c,l) => 
              let val corecur = deconstructor output
                  fun iter state [] = state
                    | iter state (arg::args) = iter (corecur state arg) args
              in case (t,c,l) of ("exp","var", [v]) => output state (str v)
                               | ("pat","bindvar", [v,_]) => output state (str v) 
                               | ("pat","alias", [v,p]) => corecur (output state (str v)) p
                               | ("atype","tyvar", [v]) => output state (str v)
                               | ("bind","lambda", [v,e]) => corecur (output state (str v)) e
                               | ("atype","forall", [v,t]) => corecur (output state (str v)) t
                               | (_,_,l) => iter state l
              end
      fun consuniq v l = if List.exists (fn x => x=v) l then l else v::l
      open Induction
   in
      val vars = induction (fromRep Deconstructor.deconstr) 
                    (toVars (dec_repr_str Repr.prt_repr)) 
                    (fn l => fn v => consuniq v l) []
   end
end

