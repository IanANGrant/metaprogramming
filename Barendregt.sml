(* signature Barendregt =
sig
   type repr
   type state
   type exstate
   val initstate : state
   val get_state : state -> exstate
   val set_state : state -> exstate -> state
   val reconstr : state -> repr -> repr * state
   val consalpha : state -> repr -> repr -> state
   val consavoid : state -> repr -> state
   val nullavoid : state -> state
end
*)

functor Barendregt
  (val module_name : string
   val map_name : string
   type repr
   eqtype str
   val incVar : str -> str
   structure Semantics : Semantics
      where type repr = repr
   structure Induction : Induction
      where type repr = repr
        and type str = str
   structure Deconstructor : Deconstructor
      where type repr = repr
   ) :> SubsConvention
         where type repr = repr 
           and type exstate = Semantics.state =
struct

   val map_name = map_name
   type repr = repr
   type exstate = Semantics.state   
   type substate = {avoid : str list,body : repr option,alpha : (str * str) list}
   type state = substate * exstate

   local
      structure Variables =
          Variables
             (type repr = repr
              type str = str
              structure Induction : Induction = Induction
              structure Deconstructor : Deconstructor = Deconstructor) :> Variables
                 where type repr = repr and type str = str

      fun errmsg function part t s l =
          let val params = t^","^s^","^(Induction.reprListToString l)
          in  module_name^"."^function^": "^part^": ("^params^")"
          end

      val vars = Variables.vars
      val str = Induction.dec_repr_str Induction.Repr.prt_repr
      val rep_str = Induction.Repr.rep_str
      fun rename v l =
          let fun iter [] = v
                | iter ((v',s)::ss) =
                     if v=v' then s else iter ss
          in iter l
          end
      fun mem s = List.exists (fn x => x=s)
      fun freshv inc v l = 
          let fun mem v l = List.exists (fn x => x=v) l
              fun iter v = if mem v l then iter (inc v) else v
          in iter (inc v)
          end
      fun freshfor inc v l e = freshv inc v ((vars e)@l)
   in
      fun get_state ((_,exstate):state) = exstate
      fun set_state ((substate,_):state) exstate  = (substate,exstate)
      val initstate = ({avoid = [],alpha = [],body = NONE}:substate,Semantics.initstate)
      fun reference ({alpha,...}:substate,exstate) v = rep_str (rename (str v) alpha)
      fun consavoid ({avoid,body,alpha}:substate,exstate) s =
         ({avoid = (str s)::avoid, body = body, alpha = alpha},exstate)
      fun nullavoid ({avoid,body,alpha}:substate,exstate) =
         ({avoid = [], body = body, alpha = alpha}:substate,exstate)
      fun consalpha ({avoid,body,alpha}:substate,exstate) s s' =
         ({avoid = avoid, body = body, alpha = (str s,str s')::alpha},exstate)
      fun binder (substate:substate as {avoid,body,alpha},exstate) v =
          let val excn = Fail (module_name^".binder: disembodied binder")
              val e = case body of SOME e => e | _ => raise excn
              val v = str v
          in if mem v avoid
                then let val v' = freshfor incVar v avoid e
                     in (rep_str v', ({avoid = avoid,
                                       body = body,
                                       alpha = (v,v')::alpha},exstate))
                     end
                else (rep_str v,(substate,exstate))
         end
      fun pattern corecur ({avoid,body,alpha}:substate,exstate) p e =
          let val state' = ({avoid = avoid,body = SOME e,alpha = alpha},exstate)
              val (p',state') = corecur state' p
              val (e',_) = corecur state' e
          in (p', e')
          end
      fun direct_binder corecur ({avoid,body,alpha}:substate,exstate) v e =
          let val state' = ({avoid = avoid,body = SOME e,alpha = alpha},exstate)
              val (v',state') = binder state' v
              val (exp,_) = corecur state' e
          in (v', exp)
          end
      fun lifted function (state:state) = fn arg =>
          let val exstate = get_state state
              val (result,exstate') = function exstate arg
          in (result,set_state state exstate')
          end
      fun reconstr deconstructor =
            fn state => fn (arg as (t,c,l)) =>
              let val corecur = deconstructor
                  fun constr state' = (* resync exstate after corecursion *)
                        lifted Semantics.reconstr (set_state state' (get_state state))
                  fun iter r state [] = (rev r,state)
                    | iter r state (arg::args) =
                        let val (e,state') = corecur state arg
                        in iter (e::r) state' args
                        end
                  val (e',state') =
                     if t = map_name then 
                        case (c,l) of
                         ("varref", [v]) =>
                             let val v' = reference state v
                             in constr state (t,c,[v'])
                             end
                       | ("patabsbind", [p,e]) =>
                             let val (p', e') = pattern corecur state p e
                             in constr state (t,c,[p',e'])
                             end
                       | ("patletbind", [p,e,e']) =>
                             let val (e'', state') = corecur state e
                                 val (p, e''') = pattern corecur state' p e'
                             in constr state' (t,c,[p,e'',e'''])
                             end
                       | ("absbind", [v,e]) =>
                             let val (v', e') = direct_binder corecur state v e
                             in constr state (t,c,[v',e'])
                             end
                       | ("letbind", [v,e,e']) =>
                             let val (e'', state') = corecur state e
                                 val (v', e''') = direct_binder corecur state' v e'
                             in constr state' (t,c,[v',e'',e'''])
                             end
                       | ("patvar", [v]) =>
                            let val (v',state') = binder state v
                            in constr state' (t,c,[v'])
                            end
                       | ("patalias", [v,p]) =>
                            let val (v',state') = binder state v
                                val (p',state'') = corecur state' p
                            in constr state'' (t,c,[v',p'])
                            end
                       | _ => let val (l',state') = iter [] state l
                              in constr state' (t,c,l')
                              end
                     else case (t,c,l) of
                               ("str","string",[s]) => (s,state)
                             | _ => let val (l',state') = iter [] state l
                                    in constr state' (t,c,l')
                                    end handle Fail s =>
                                            raise Fail (errmsg "reconstr'" ("excn: "^s) t c l)
              in (e',state')
              end
      local
         fun loopback deconstruct reconstruct =
             fn state => fn input =>
                 let val (output,state') = deconstruct state input
                 in reconstruct state' output
                 end
         val deconstruct = loopback (lifted Semantics.deconstr)
         val reconstruct = reconstr
      in
         val reconstr = Induction.induction deconstruct reconstruct
      end
   end
end
