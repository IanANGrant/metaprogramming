signature OccursCheck =
sig
    type repr
    exception Occurs
    val check : repr -> repr -> bool
end

functor OccursCheck
  (type repr
   val map_name : string
   structure Induction : Induction
       where type repr = repr
   structure Deconstructor : Deconstructor
       where type repr = repr) :> OccursCheck
   where type repr = repr =
struct
   type repr = repr
   exception Occurs
   local
      fun toOccurs str deconstructor =
         fn var => fn state => fn (t,c,l) =>
            state orelse
              let val corecur = deconstructor var
                  fun iter state [] = state
                    | iter state (arg::args) =
                          iter (corecur state arg) args 
              in if t = map_name 
                    then case (c,l) of
                            ("varref",[v]) =>
                                  if (str v) = (str var)
                                     then true
                                     else false
                          | _ => iter state l
                    else iter state l
              end
      open Induction
   in
      fun check v = induction (fromRep Deconstructor.deconstr) 
                      (toOccurs (dec_repr_str Repr.prt_repr))
                       v false
   end
end
