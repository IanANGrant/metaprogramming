signature Substitute =
sig
   type repr
   type state
   type exstate
   val map_name : string
   val initstate : state
   val get_state : state -> exstate
   val set_state : state -> exstate -> state
   val reconstr : state -> string * string * repr list -> repr * state
end

functor Substitute
  (val module_name : string
   type repr
   type state
   val map_name : string
   val initstate : state
   eqtype str
   val incVar : str -> str
   structure Semantics : Semantics
      where type repr = repr
   structure SubsSemantics : SubsSemantics
      where type repr = repr
   structure SubsConvention : SubsConvention
      where type repr = repr
        and type exstate = SubsSemantics.state
   structure FreeVariables : FreeVariables
      where type repr = repr
        and type str = str
   structure Induction : Induction
      where type repr = repr
        and type str = str
   structure Deconstructor : Deconstructor
       where type repr = repr) :> Substitute
   where type repr = repr
     and type exstate = state =
struct
   type repr = repr
   type exstate = state
   local
      fun errmsg function msg t s l =
         let val params = t^","^s^","^(Induction.reprListToString l)
         in  module_name^"."^function^": "^msg^": ("^params^")"
         end

      val rep_str = Induction.Repr.rep_str

      val r_rr = Induction.dec_repr_repr Induction.Repr.prt_repr
      val prt_reprrepr = Induction.ReprRepr.prt_reprrepr
      val reprnil = Induction.Repr.rep_reprrepr (Induction.ReprRepr.rep_nil ())
      fun reprcons a l = Induction.Repr.rep_reprrepr (Induction.ReprRepr.rep_repr a l)

      fun foldright f =
          let val rec iter = fn r => prt_reprrepr
                  (fn () => r)
                  (fn a => fn b => iter (f a r) (r_rr b))
          in iter
          end

      fun foldleft f =
          let val rec iter = fn r => prt_reprrepr
                  (fn () => r)
                  (fn a => fn b => f a (iter r (r_rr b)))
          in iter
          end
   in
      type state = SubsConvention.state * exstate

      fun get_state ((_,exstate):state) = exstate
      fun set_state ((state,_):state) exstate  = (state,exstate)

      val initstate = (SubsConvention.initstate,initstate)
      val map_name = map_name
      fun reconstr ((state,exstate):state) (input as (t,c,l)) =
         let (* val _ = print ((errmsg "reconstr" "args" t c l)^"\n") *)
             fun subs state e = SubsConvention.reconstr state e
             fun getsubs state =
                 let val (result,sstate) = SubsSemantics.get_subs (SubsConvention.get_state state)
                     val state' = SubsConvention.set_state state sstate
                 in (result,state')
                 end
             fun nullsubs state = SubsConvention.set_state state (SubsSemantics.initstate)
             fun addsubs state v e =
                 let val fvs = FreeVariables.freevars e
                     val state' = List.foldr
                           (fn (v,state) => SubsConvention.consavoid state (rep_str v)) state fvs
                     val sstate = SubsSemantics.set_subs (SubsConvention.get_state state') v e
                     val state'' = SubsConvention.set_state state' sstate
                 in (reprnil,state'')
                 end
             fun addalpha state v v' =
                 let val state' = SubsConvention.consalpha state v v'
                 in (reprnil,state')
                 end
             val reverse = (foldright reprcons reprnil) o r_rr
             fun revsubs state =
                 let fun errexcn s = Fail (errmsg (module_name^".revsubs") s t c l)
                     val (ss,_) = getsubs state
                     val state' = nullsubs state
                     val ss' = reverse ss
                     val state'' = foldright
                           (fn a => fn r =>
                                 let val (v',e') = Induction.ReprRepr.prt_reprrepr
                                                    (fn () => raise (errexcn "nil pair"))
                                                    (fn a => fn b => (a,b)) (r_rr a)
                                     val (_,r') = addsubs r v' e'
                                 in r' end) state' (r_rr ss')
                 in (reprnil,state'') end
             fun subsaddsubs state v e =
                 let fun errexcn s = Fail (errmsg (module_name^".subsaddsubs") s t c l)
                     val (ss,_) = getsubs state
                     val state' = nullsubs state
                     val (_,state'') = addsubs state' v e
                     val state''' = foldright
                           (fn a => fn r =>
                                 let val (v',e') = Induction.ReprRepr.prt_reprrepr
                                                    (fn () => raise (errexcn "nil pair"))
                                                    (fn a => fn b => (a,b)) (r_rr a)
                                     val (e'',_) = subs state'' e'
                                     val (_,r') = addsubs r v' e''
                                 in r' end) state' (r_rr ss)
                     val (_,state'4) = addsubs state''' v e
                 in
                    revsubs state'4
                 end
             fun composesubs state ss =
                 let fun errexcn s = Fail (errmsg (module_name^".composesubs") s t c l)
                     val state' = foldright
                           (fn a => fn r =>
                              let val (v,e) = Induction.ReprRepr.prt_reprrepr
                                               (fn () => raise (errexcn "nil pair"))
                                               (fn a => fn b => (a,b)) (r_rr a)
                                  val (_,r) = subsaddsubs r v e
                              in r end) state (r_rr ss)
                 in (reprnil,state')
                 end
             val (res,state) =
               if t = map_name 
                  then case (c,l) of
                         ("subs",[e]) => subs state e
                       | ("alpha",[v,v']) => addalpha state v v'
                       | ("parallel",[v,e]) => addsubs state v e
                       | ("composed",[v,e]) => subsaddsubs state v e
                       | ("getsubs",[]) => getsubs state
                       | ("compose",[ss]) => composesubs state ss
                       | _ => raise Fail (errmsg (module_name^".reconstr") "no case" t c l)
                  else raise Fail (errmsg (module_name^".reconstr") ("wrong type (map: "^map_name^")") t c l)
         in (res,(state,exstate))
         end
   end
end
