signature SubsSemantics =
sig
   type repr
   type state
   type exstate
   val initstate : state
   val map_name : string
   val get_state : state -> exstate
   val set_state : state -> exstate -> state
   val set_subs : state -> repr -> repr -> state
   val get_subs : state -> repr * state
   val deconstr : state -> repr -> (string * string * repr list) * state
   val reconstr : state -> string * string * repr list -> repr * state
end

functor SubsSemantics
  (val module_name : string
   type repr
   structure Semantics : Semantics
      where type repr = repr
   structure Induction : Induction
      where type repr = repr) :> SubsSemantics
   where type repr = repr 
     and type exstate = Semantics.state =
struct
   local
      val rep_str = Induction.Repr.rep_str
      val str = Induction.dec_repr_str Induction.Repr.prt_repr
      fun mem s = List.exists (fn x => x=s)
      fun lookup v l =
          let fun iter [] = NONE
                | iter ((v',s)::ss) =
                     if v=v' then SOME s else iter ss
          in iter l
          end
      val reprnil = Induction.Repr.rep_reprrepr (Induction.ReprRepr.rep_nil ())
      fun reprcons a l = Induction.Repr.rep_reprrepr (Induction.ReprRepr.rep_repr a l)
   in
      type repr = repr
      type exstate = Semantics.state
      type str = Induction.str
      type state = {bound : str list, subs : (str * repr) list} * exstate

      val initstate = ({bound = [],subs = []},Semantics.initstate)
      val map_name = Semantics.map_name

      fun get_state ((_,exstate):state) = exstate
      fun set_state ((state,_):state) exstate = (state,exstate)

      fun set_bound (({bound,subs},exstate):state) sr = ({bound = (str sr)::bound,subs=subs},exstate)
      fun get_bound (({bound,subs},exstate):state) sr = mem (str sr) bound
      fun set_subs  (({bound,subs},exstate):state) sr er = ({bound = bound,subs = (str sr,er)::subs},exstate)
      fun get_subs  (state as (({bound,subs},exstate):state)) = 
                     (List.foldl (fn ((v,e),r) => reprcons (reprcons (rep_str v) e) r) reprnil subs, state)
      fun lookup_subs (({bound,subs},exstate):state) sr = lookup (str sr) subs

      fun lifted function state input =
         let val (r,s) = function (get_state state) input
         in (r, set_state state s)
         end

      fun reconstr state (cmd as (t,c,l)) =
         let val reconstr = lifted Semantics.reconstr
         in if t = map_name
               then case (c,l) of
                    ("varref",v::args) =>
                           (case lookup_subs state v of
                              SOME e =>
                                  if not (get_bound state v)
                                     then reconstr state (map_name,"id",[e])
                                     else reconstr state cmd
                            | NONE => reconstr state cmd)
                     | _ => reconstr state cmd
               else reconstr state cmd
        end

      fun deconstr state repr =
         let val (result as (cmd as (t,c,l),state')) = lifted Semantics.deconstr state repr
             fun binder v = 
                 let val state'' = 
                       case lookup_subs state' v of
                           SOME _ => set_bound state' v
                         | NONE => state'
                 in (cmd,state'')
                 end
         in if t = map_name
               then case (c,l) of
                    ("absbind",[v,_]) => binder v
                  | ("letbind",[v,_,_]) => binder v
                  | ("patvar",[v]) => binder v
                  | _ => (cmd,state')
               else (cmd,state')
         end
   end
end
