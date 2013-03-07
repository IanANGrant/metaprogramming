functor TypeCheckSemantics
  (val module_name : string
   val map_name : string
   type repr
   structure Semantics : Semantics
      where type repr = repr
   structure Constructor : Constructor
      where type repr = repr
   structure Induction : Induction
      where type repr = repr) :> Semantics
   where type repr = repr =
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
   in
      type repr = repr
      type exstate = Semantics.state
      type str = Induction.str
      type state = unit * exstate
      val map_name = Semantics.map_name

      val initstate = ((),Semantics.initstate)

      fun get_state ((_,exstate):state) = exstate
      fun set_state ((state,_):state) exstate = (state,exstate)

      fun errmsg function part t s l =
          let val params = t^","^s^","^(Induction.reprListToString l)
          in  module_name^"."^function^": "^part^": ("^params^")"
          end

      fun lifted function state input =
         let val (r,s) = function (get_state state) input
         in (r, set_state state s)
         end

      fun reconstr state (cmd as (t,c,l)) =
         let val reconstr = lifted Semantics.reconstr
         in if t = map_name
               then case (c,l) of
                    ("varref",v::args) => reconstr state cmd
                     | _ => reconstr state cmd
               else reconstr state cmd
        end

      fun deconstr state repr =
         let val deconstr = lifted Semantics.deconstr
             val (result as (cmd as (t,c,l),state')) = deconstr state repr
             fun foldl state r ("infixop","cons",[e,e']) = 
                    let val (de',state') = deconstr state e'
                    in foldl state' (Constructor.constr ("exp","comb",[r,e])) de'
                    end
               | foldl state r ("infixop","exp",[e]) = ((map_name,"comb",[r,e]),state)
               | foldl _ _ (t',c',l') = raise Fail (errmsg "infixopexp" "no case" t' c' l')
             fun infixopexp state e =
                let val (arg as (t',c',l'),state'') = deconstr state e
                in case arg of
                      ("infixop","cons",[e,e']) =>
                           let val (e',state'') = deconstr state e
                           in foldl state'' e e'
                           end
                    | ("infixop","exp",[e]) => ((map_name,"id",[e]),state'')
                    | _ => raise Fail (errmsg "infixopexp" "no case" t' c' l')
                end
            fun combexp state e e' =
                let val (arg as (t',c',l'),state'') = deconstr state e'
                in case arg of
                      ("exp","atypeexp",[t]) => ((map_name,"uapp",[e,t]),state'')
                    | _ => ((map_name,"comb",[e,e']),state'')
                end
         in case (t,c,l) of (* Should use MapJoinSemantics.sml *)
                    ("pat","patvar", [v,t]) => ((map_name,"patvar",[v,t]),state')
                  | ("pat","wildcard",[t]) => ((map_name,"wildcard",[t]),state')
                  | ("pat","pair",[p,p']) => ((map_name,"patpair",[p,p']),state')
                  | ("bind","let",[p,e,e']) => ((map_name,"patletbind",[p,e,e']),state')
                  | ("bind","abs",[p,e]) => ((map_name,"patabsbind",[p,e]),state')
                  | ("bind","lambda",[p,e]) => ((map_name,"patabsbind",[p,e]),state')
                  | ("exp","var",[v]) => ((map_name,"varref",[v]),state')
                  | ("exp","comb",[e,e']) => combexp state' e e'
                  | ("exp","pairexp",[e,e']) => ((map_name,"pairexp",[e,e']),state')
                  | ("exp","infixopexp",[e]) => infixopexp state' e
                  | ("exp","atypeexp",[e]) => ((map_name,"atypeexp",[e]),state')
                  | ("exp","bindexp",[e]) => ((map_name,"id",[e]),state')
                  | _ => (cmd,state')
         end
   end
end
