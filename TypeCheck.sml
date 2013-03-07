signature TypeCheck =
sig
   type repr
   exception TypeCheck of string
   val typecheck : repr -> repr
end

functor TypeCheck
  (val module_name : string
   type repr
   eqtype str
   type state
   val incVar : str -> str
   val atype_eq : repr -> repr -> bool
   val dec_function_type : (repr * repr -> 'a) -> repr -> 'a
   val dec_forall_type : (repr * repr -> 'a) -> repr -> 'a
   val initstate : state
   structure Induction : Induction
       where type repr = repr
         and type str = str
   structure FreeVariables : FreeVariables
       where type repr = repr
         and type str = str
   structure Semantics : Semantics
       where type repr = repr
         and type state = state
    ) :> TypeCheck
         where type repr = repr =
struct
   type repr = repr
   type exstate = state
   type str = str
   type state = {env : (str * repr) list} * exstate

   exception TypeCheck of string

   fun get_state ((_,exstate):state) = exstate
   fun set_state ((state,_):state) exstate = (state,exstate)
   val initstate = ({env = []},initstate)

   local
      val str = Induction.dec_repr_str Induction.Repr.prt_repr
      val rep_str = Induction.Repr.rep_str
      val reprToString = Induction.reprToString
      val reprListToString = Induction.reprListToString
      fun mem s = List.exists (fn x => x=s)
      fun lookup v l =
          let fun iter [] = NONE
                | iter ((v',s)::ss) =
                     if v=v' then SOME s else iter ss
          in iter l
          end

      fun set_env (({env},exstate):state) s er = ({env = (s,er)::env},exstate)
      fun get_env (({env},exstate):state) s = lookup s env

      fun errmsg function part t s l =
          let val params = t^","^s^","^(reprListToString l)
          in  module_name^"."^function^": "^part^": ("^params^")"
          end

      val freevars = FreeVariables.freevars
      val freein = FreeVariables.freein

      fun lifted function state = fn repr =>
          let val exstate = get_state state
              val (result,exstate') = function exstate repr
          in (result,set_state state exstate')
          end

      val reconstr = lifted Semantics.reconstr
      val deconstr = lifted Semantics.deconstr

      fun lambda_type serrmsg state v t e =
         let fun uabscheck state v e =
            let val fvs = freevars e
                fun typeof v =
                       case get_env state v of
                           NONE => raise Fail (serrmsg ("internal error: "^
                                               (Induction.Str.toMLString v)^" not bound"))
                         | SOME t => t
                val tel = List.map (fn v => (typeof v,v)) fvs
                val ftel = List.filter (fn (t,v) => freein v t) tel
                val res = List.map (fn (t,v) => ((reprToString (rep_str v))^":"^(reprToString t))) ftel
                val slist = List.foldl (fn (s,r) => r^(if r = "" then "" else ",")^s) ""
            in case res of [] => NONE
                         | _ => SOME (slist res)
            end
         in
            case uabscheck state v e of
               SOME s => raise TypeCheck (serrmsg ("free variable "^s))
             | _ => reconstr state ("typecheck","forall",[v,t])
         end
      fun function_type state t t' = reconstr state ("typecheck","function",[t,t'])
      fun pair_type state t t' = reconstr state ("typecheck","pair",[t,t'])
      fun comb_type serrmsg state a b =
         let val atype = Induction.dec_repr_atype Induction.Repr.prt_repr
             val t' = (dec_function_type
                         (fn (t,t') =>
                             if atype_eq t b
                                then t'
                                else raise (TypeCheck (serrmsg ("comb: wrong type argument: "^
                                           (reprListToString [a,b]))))) a)
                                                 handle Fail s =>
                                                       raise (TypeCheck (serrmsg
                                                               ("comb: wrong type to apply: "^
                                                                 (reprListToString [a,b]))))
         in (t',state)
         end
      fun uapp_type serrmsg state a b =
         dec_forall_type (fn (v,t) => reconstr state ("typecheck","substitute",[v,t,b])) a
                            handle Fail _ =>
                                raise (TypeCheck (serrmsg ("uapp: wrong type argument: "^
                                                   (reprListToString [a,b]))))
      fun id state t = reconstr state ("typecheck","id",[t])
      fun reconstruct deconstructor =
           fn state => fn (arg as (t,c,l)) =>
              let val corecur = deconstructor
                  fun serrmsg s = errmsg "reconstr" s t c l
                  val (e',state') =
                     (case t of "typecheck" => (case (c,l) of
                          ("patvar", [v,t]) => id (set_env state (str v) t) t
                        | ("wildcard", [t]) => id state t
                        | ("id", [e]) => corecur state e
                        | ("varref", [v]) => 
                              (case get_env state (str v) of
                                  SOME t => id state t
                                | _ => raise (TypeCheck 
                                          (errmsg "reconstr" ("free variable "^
                                            (Induction.Str.toMLString (str v))) t c l)))
                        | ("patletbind", [p,e,e']) =>
                             let val (ty, state') = corecur state e
                                 val (t',state'') = corecur state' p
                             in if atype_eq ty t'
                                   then corecur state'' e'
                                   else raise (TypeCheck 
                                                  (errmsg "reconstr" "wrong bind type" t c l))
                             end
                        | ("patabsbind", [p,e]) =>
                             let val (t, state') = corecur state p
                                 val (t',state'') = corecur state' e
                             in function_type state'' t t'
                             end
                        | ("patpair", [p,p']) =>
                             let val (t, state') = corecur state p
                                 val (t',state'') = corecur state' p'
                             in pair_type state'' t t'
                             end
                        | ("patalias", [v,p]) =>
                             let val (t, state') = corecur state p
                             in id (set_env state' (str v) t) t
                             end
                        | ("lambda", [v,e]) =>
                             let val (ty, state') = corecur state e
                             in lambda_type serrmsg state' v ty e
                             end
                        | ("comb", [e,e']) =>
                             let val (t', state') = corecur state e
                                 val (t'',state'') = corecur state' e'
                             in comb_type serrmsg state'' t' t''
                             end
                        | ("uapp", [e,t']) =>
                             let val (t'', state') = corecur state e
                             in uapp_type serrmsg state' t'' t'
                             end
                        | ("pairexp", [e,e']) =>
                             let val (t', state') = corecur state e
                                 val (t'',state'') = corecur state' e'
                             in pair_type state'' t' t''
                             end
                        | (_,[t]) => id state t
                        | _ => raise Fail (errmsg "reconstr" "no case" t c l))
                      | _ => raise Fail (errmsg "reconstr" "no case" t c l))
              in (e',state')
              end
   in
      fun typecheck e =
         let fun loopback deconstruct reconstruct =
               fn state => fn input =>
                  let val (output,state') = deconstruct state input
                  in reconstruct state' output
                  end
             val deconstruct = loopback deconstr
             val (result,_) = Induction.induction deconstruct reconstruct initstate e
         in result
         end
   end
end
