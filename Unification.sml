signature Unify =
sig
   type repr
   type state
   type exstate
   exception Unify of string * ((repr * repr) list * state)
   val initstate : state
   val get_state : state -> exstate
   val set_state : state -> exstate -> state
   val reconstr : state -> repr -> repr * state
end

functor Unify
  (val module_name : string
   type repr
   val map_name : string
   structure Induction : Induction
       where type repr = repr
   structure Constructor : Constructor
      where type repr = repr
   structure Deconstructor : Deconstructor
      where type repr = repr
   structure Occurs : OccursCheck
      where type repr = repr
   structure Semantics : Semantics
      where type repr = repr) :> Unify
         where type repr = repr 
           and type exstate = Semantics.state =
struct
   type repr = repr
   type exstate = Semantics.state   
   type unistate = {assigned : string list}
   type state = unistate * exstate
   exception Unify of string * ((repr * repr) list * state)

   val toString = Induction.Str.toMLString o (Induction.dec_repr_str Induction.Repr.prt_repr)

   fun get_state ((_,exstate):state) = exstate
   fun set_state ((unistate,_):state) exstate = (unistate,exstate)

   val initstate = ({assigned = []}:unistate,Semantics.initstate)

   local
      fun errmsg function part t s l =
          let val params = t^"."^s^","^(Induction.reprListToString l)
          in  module_name^"."^function^": "^part^": ("^params^")"
          end

      fun errmsg2 function part t s l t' s' l' =
          let val params1 = t^"."^s^","^(Induction.reprListToString l)
              val params2 = t'^"."^s'^","^(Induction.reprListToString l')
          in  module_name^"."^function^": "^part^": ("^params1^", "^params2^")"
          end

      fun lifted function (state:state) = fn arg =>
          let val exstate = get_state state
              val (result,exstate') = function exstate arg
          in (result,set_state state exstate')
          end

      val str = Induction.dec_repr_str Induction.Repr.prt_repr
      val reprnil = Induction.Repr.rep_reprrepr (Induction.ReprRepr.rep_nil ())

      fun reconstr deconstructor =
            fn state => fn (arg as (t,c,l)) =>
              let val corecur = deconstructor
              (*  val _ = print((module_name^".reconstr: "^t^"."^c^" "^
                                               (Induction.reprListToString l)^"\n")) *)
                  val constr = lifted Semantics.reconstr

                  fun substitute state e =
                      let val (r,_) = constr state ("substitute","subs",[e])
                      in r end

                  fun addalpha state v v' =
                      let val (_,state') = constr state ("substitute","alpha",[v,v'])
                       (* val _ = print("Unification.addalpha: "^
                                       (Induction.reprListToString [v,v'])^"  ("^t^"."^c^" "^
                                       (Induction.reprListToString l)^")\n") *)
                      in state'
                      end
                  fun add_assigned (({assigned},extstate):state) s s' =
                        ({assigned=(toString s)::assigned},extstate)

                  fun add_assign state s s' =
                      let (* val _ = print("Unification.add_assign: "^(toString s)^"/"^(toString s')^"\n") *)
                          val state' = if (str s) = (str s') 
                                          then state else add_assigned state s s'
                          val state'' = if (str s) = (str s') 
                                           then state'
                                           else addalpha state' s s'
                      in state''
                      end

                  fun is_assigned (({assigned},_):state) s = 
                        List.exists (fn x' => x' = (toString s)) assigned

                  fun subst state v e e' =
                      if is_assigned state v
                         then raise Unify ("assigned",([(e',v)],state))
                         else let val (_,state') = constr state ("substitute","composed",[v,e])
                              in constr state' ("substitute","subs",[e'])
                              end

                  fun unify_args state (e1',l1s,e2',l2s) =
                      let (* val _ =  print("Unification.unify_args: unifying ["^
                                                (Induction.reprListToString (e1'::l1s))^", "^
                                                (Induction.reprListToString (e2'::l2s))^"]\n") *)
                          val e1'' = substitute state e1'
                          val e2'' = substitute state e2'
                       (* val _ =  print("Unification.unify_args: e'',e2''= "^
                                                (Induction.reprToString e1'')^", "^
                                                (Induction.reprToString e2'')^"\n") *)
                          val r = Constructor.constr (map_name,"unify",
                                      [e1'',e2''])
                       (* val _ =  print("Unification.unify_args: subst "^
                                                (Induction.reprToString r)^"\n") *)
                          val (r',state') = corecur state r
                          val r'' = Constructor.constr (map_name,"unify",
                                      [Constructor.constr (map_name,"arglist", l1s),
                                       Constructor.constr (map_name,"arglist", l2s)])
                          val (_,state'') = corecur state' r''
                      in state''
                      end

                  val (e',state') =
                     if t = map_name then
                        case (c,l) of
                         ("unify",[e1,e2]) =>
                           let (* val _ = print("Unification.reconstr: deconstructing ["^
                                                (Induction.reprToString e1)^", "^
                                                (Induction.reprToString e2)^"]\n") *)
                               val ((t1,c1,l1),_) = Semantics.deconstr Semantics.initstate e1
                               val ((t2,c2,l2),_) = Semantics.deconstr Semantics.initstate e2 
                           (*  val (t1,c1,l1) = ReprEmbedding.Deconstructor.deconstr e1
                               val (t2,c2,l2) = ReprEmbedding.Deconstructor.deconstr e2 *)
                           (*  val _ = print("Unification.reconstr: unify "^
                                                t1^"."^c1^" "^(Induction.reprListToString l1)^" with "^
                                                t2^"."^c2^" "^(Induction.reprListToString l2)^"\n") *)
                           in
                             case ((t1,c1,l1),(t2,c2,l2)) of
                                ((_,"varref",v1::l1'), (_,"varref",v2::l2')) =>
                                    if (str v1) = (str v2) andalso t1=t2
                                       then case (l1',l2') of
                                               ([],[]) => (e1,state)
                                             | (e1'::l1s,e2'::l2s) => 
                                                  let val state' = unify_args state (e1',l1s,e2',l2s)
                                                  in constr state' ("substitute","subs",[e1])
                                                  end
                                             | _ => raise Fail 
                                                (errmsg2 "reconstr" "no (varref=) case" t1 c1 l1' t2 c2 l2')
                                       else subst state v2 e1 e2
                              | ((_,"absbind",v1::l1'), (_,"absbind",v2::l2')) =>
                                    (case (l1',l2') of
                                           (e1'::l1s,e2'::l2s) => 
                                              let val state' = add_assign state v1 v2
                                                  val state'' = unify_args state' (e1',l1s,e2',l2s)
                                              in constr state'' ("substitute","subs",[e1])
                                              end
                                          | _ => raise Fail 
                                            (errmsg2 "reconstr" "no (absbind) case" t1 c1 l1' t2 c2 l2'))
                              | ((_,"patvar",v1::l1'), (_,"patvar",v2::l2')) =>
                                 let (* val _ = print("Unification.reconstr: patvar1 "^
                                                t1^"."^c1^" "^(Induction.reprListToString l1')^" and "^
                                                t2^"."^c2^" "^(Induction.reprListToString l2')^"\n") *)
                                     val state' = add_assign state v1 v2
                                     (* val _ = print("Unification.reconstr: patvar2 "^
                                                t1^"."^c1^" "^(Induction.reprListToString l1')^" and "^
                                                t2^"."^c2^" "^(Induction.reprListToString l2')^"\n") *)
                                 in case (l1',l2') of
                                            ([],[]) => (reprnil,state')
                                          | (e1'::l1s,e2'::l2s) => 
                                              let val state'' = unify_args state' (e1',l1s,e2',l2s)
                                              in constr state'' ("substitute","subs",[e1])
                                              end
                                          | _ => raise Fail 
                                            (errmsg2 "reconstr" "no (patvar) case" t1 c1 l1' t2 c2 l2')
                                 end
                              | ((_,"varref",v1::_), (_,_,[])) => subst state v1 e2 e1
                              | ((_,_,[]),(_,"varref",v2::_)) => subst state v2 e1 e2
                              | ((_,"varref",v1::_), (_,_,_)) =>
                                  (*  if Occurs.check v1 e2
                                       then raise Unify ("occurs",([(e2,v1)],state))
                                       else *) subst state v1 e2 e1
                              | ((_,_,_),(_,"varref",v2::_)) =>
                                    (* if Occurs.check v2 e1
                                       then raise Unify ("occurs",([(e1,v2)],state))
                                       else*) subst state v2 e1 e2
                              | ((t1,"arglist",l1'),(t2,"arglist",l2')) =>
                                    if t1 = t2 andalso t1 = map_name
                                       then case (l1',l2') of
                                               ([],[]) => (reprnil,state)
                                             | (e1'::l1s,e2'::l2s) => 
                                                   (reprnil,unify_args state (e1',l1s,e2',l2s))
                                             | _ => raise Fail 
                                                (errmsg2 "reconstr" "no (arglist) case" t1 c1 l1' t2 c2 l2')
                                       else raise Fail (errmsg2 "reconstr" "arglist" t1 c1 l1' t2 c2 l2')
                              | ((t1,c1,[]),(t2,c2,[])) =>
                                       if (t1,c1) = (t2,c2)
                                       then (e1,state)
                                       else raise Unify ("constant", ([(e1,e2)],state))
                              | ((t1,"id",[s]),(t2,"id",[s'])) =>
                                       if t1 = t2 andalso (str s) = (str s')
                                       then (reprnil,state)
                                       else raise Unify ("constant", ([(e1,e2)],state))
                              | ((t1,c1,l1'),(t2,c2,l2')) =>
                                       if (t1,c1) = (t2,c2)
                                       then case (l1',l2') of
                                               ([],[]) => (reprnil,state)
                                             | (e1'::l1s,e2'::l2s) => 
                                                  let val state' = unify_args state (e1',l1s,e2',l2s)
                                                  in constr state' ("substitute","subs",[e1]) end
                                             | _ => raise Fail
                                                (errmsg2 "reconstr" "no (const) case" t1 c1 l1' t2 c2 l2')
                                       else raise Unify ("constant", ([(e1,e2)],state))
                           end
                        | _ => raise Fail (errmsg "reconstr" "no (non unify) case" t c l)
                     else raise Fail (errmsg "reconstr" ("no case ("^map_name^")") t c l)
              in (e',state')
              end

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
