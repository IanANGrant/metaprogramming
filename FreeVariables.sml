signature FreeVariables =
sig
    type repr
    eqtype str
    val freevars : repr -> str list
    val freein : str -> repr -> bool
end

functor FreeVariables
  (type repr
   eqtype str
   structure Induction : Induction
       where type repr = repr and type str = str
   structure Deconstructor : Deconstructor
       where type repr = repr) :> FreeVariables
   where type repr = repr and type str = str =
struct
   type repr = repr
   type str = str
   local
      type freestate = {free: str list, bound : str list}
      type freestatefns = {addfree : freestate -> str -> freestate,
                           addbound : freestate -> str -> freestate,
                           binder : (freestate -> repr -> freestate) ->
                                     freestate -> repr -> repr -> freestate,
                           direct_binder : (freestate -> repr -> freestate) ->
                                     freestate -> str -> repr -> freestate}
      structure BoundVariables =
         BoundVariables
            (type repr = repr
             type str = str
             structure Induction : Induction = Induction
             structure Deconstructor : Deconstructor = Deconstructor
            ) :> BoundVariables
                where type repr = repr and type str = str

      fun addfree {free,bound} v =
         let fun mem s = List.exists (fn x => x=s)
             val free = if mem v bound then free else v::free
         in
            {free = free, bound = bound}
         end
      fun addbound {free,bound} v = {free = free,
                                     bound = v::bound}
      fun direct_binder corecur (state:freestate) v e =
         let val state':freestate = addbound state v
             val state'' = corecur state' e
         in {free = #free state'', bound = #bound state}
         end
      fun binder corecur (state:freestate) p e =
         let val bvars = BoundVariables.boundvars (#bound state) p
             val state':freestate = {free = #free state, bound = bvars}
             val state' = corecur state' e
         in {free = #free state', bound = #bound state}
         end
         val freeVarsState:freestate = {free=[], bound=[]}
         val freeVarsFns:freestatefns = {addfree=addfree,addbound=addbound,binder=binder,
                                         direct_binder=direct_binder}
   
      fun toFreeVars str deconstructor =
         fn output:freestatefns => fn state => fn (t,c,l) => 
              let val corecur = deconstructor output
                  val addfree = #addfree output
                  val binder = #binder output corecur
                  val direct_binder = #direct_binder output corecur
                  fun iter state [] = state
                    | iter state (arg::args) =
                           iter (corecur state arg) args
              in case (t,c,l) of
                   ("exp","var", [v]) => addfree state (str v)
                 | ("bind","abs", [p,e]) => binder state p e
                 | ("atype","tyvar", [v]) => addfree state (str v)
                 | ("atype","forall", [v,t]) => direct_binder state (str v) t
                 | ("bind","lambda", [v,e]) => direct_binder state (str v) e
                 | ("bind","let", [p,e,e']) => binder (corecur state e) p e'
                 | (_,_,l) => iter state l
              end
      open Induction
   in
      fun freevars e = 
         let val {free,...} =
                induction (fromRep Deconstructor.deconstr)
                       (toFreeVars (dec_repr_str Repr.prt_repr))
                        freeVarsFns freeVarsState e
         in free
         end
      fun freein v e = List.exists (fn x => x=v) (freevars e)
   end
end
