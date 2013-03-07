functor SimpleSubs
  (val module_name : string
   type repr
   val map_name : string
   structure Semantics : Semantics
      where type repr = repr
   structure Induction : Induction
      where type repr = repr
  ) :> SubsConvention
         where type repr = repr 
           and type exstate = Semantics.state =
struct
   val map_name = map_name
   type repr = repr
   type exstate = Semantics.state
   type str = Induction.str
   type substate = {alpha : (str * str) list}
   type state = substate * exstate

   local
      fun errmsg function part t s l =
          let val params = t^","^s^","^(Induction.reprListToString l)
          in  module_name^"."^function^": "^part^": ("^params^")"
          end

      val rep_str = Induction.Repr.rep_str
      val str = Induction.dec_repr_str Induction.Repr.prt_repr
      val toMLString = Induction.Str.toMLString
      fun rename v l =
          let fun iter [] = v
                | iter ((v',s)::ss) =
                     if v=v' then s else iter ss
          in iter l
          end
   in
      fun get_state ((_,exstate):state) = exstate
      fun set_state ((substate,_):state) exstate  = (substate,exstate)
      val initstate = ({alpha = []},Semantics.initstate)
      fun consavoid (state:state) (_:repr) = state
      fun nullavoid (state:state) = state
      fun consalpha ({alpha}:substate,exstate) s s' =
              let val _ = () (*print("consalpha: renaming "^(toMLString (str s))^
                                                      "/"^(toMLString (str s'))^"\n") *)
              in ({alpha = (str s,str s')::alpha},exstate) end
      fun reference ({alpha},exstate) v = rep_str (rename (str v) alpha)
      fun binder (substate:substate as {alpha},exstate) v =
          let val v = str v
              val v' = rename v alpha
(*              val _ = print("binder: renamed "^(toMLString v')^
                                         "/"^(toMLString v)^"\n") *)
          in (rep_str v',(substate,exstate))
          end
      fun pattern corecur state p e =
          let val (p',state') = corecur state p
              val (e',_) = corecur state' e
          in (p', e')
          end
      fun direct_binder corecur ({alpha}:substate,exstate) v e =
          let val state' = ({alpha = alpha},exstate)
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
                  fun constr state' = (* resync exstate after corecursion (FIXME) *)
                        lifted Semantics.reconstr (set_state state' (get_state state))
                  fun iter r state [] = (rev r,state)
                    | iter r state (arg::args) =
                        let val (e,state') = corecur state arg
                        in iter (e::r) state' args
                        end
                  fun default state t c l =
                         let val (l',state') = iter [] state l
                         in constr state' (t,c,l')
                         end
              in if t = map_name 
                    then case (c,l) of
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
                       | ("patvar", v::args) =>
                            let val (v',state') = binder state v
                                val (args',state'') = iter [] state' args
                            in constr state'' (t,c,v'::args')
                            end
                       | ("patalias", [v,p]) =>
                            let val (v',state') = binder state v
                                val (p',state'') = corecur state' p
                            in constr state'' (t,c,[v',p'])
                            end
                       | ("id", l) => constr state (t,c,l)
                       | _ => raise Fail (errmsg "reconstr" "no case" t c l) (* default state t c l *)
                    else default state t c l
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
