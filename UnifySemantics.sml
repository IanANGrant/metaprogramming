signature UnifySemantics =
sig
   type repr
   type state
   type exstate
   val initstate : state
   val map_name : string
   val get_state : state -> exstate
   val get_mgu : state -> repr
   val set_state : state -> exstate -> state
   val deconstr : state -> repr -> (string * string * repr list) * state
   val reconstr : state -> string * string * repr list -> repr * state
end

functor UnifySemantics
  (val module_name : string
   type repr
   type state
   structure Substitute : Substitute
      where type repr = repr
   structure Semantics : Semantics
      where type repr = repr
   structure Deconstructor : Deconstructor
      where type repr = repr
   structure Induction : Induction
      where type repr = repr) :> UnifySemantics
   where type repr = repr 
     and type exstate = Semantics.state
=
struct
   local
      fun errmsg function part t s l =
          let val params = t^"."^s^","^(Induction.reprListToString l)
          in  module_name^"."^function^": "^part^": ("^params^")"
          end
   in
      type repr = repr
      type exstate = Semantics.state
      type str = Induction.str
      type state = Substitute.state * exstate

      val initstate = (Substitute.initstate,Semantics.initstate)
      val map_name = Semantics.map_name

      fun get_state ((_,exstate):state) = exstate
      fun set_state ((state,_):state) exstate = (state,exstate)

      fun get_mgu ((subsstate,exstate):state) = 
             let val (mgu,sstate') = Substitute.reconstr subsstate (Substitute.map_name,"getsubs",[])
             in mgu
             end

      fun lifted function state input =
         let val (r,s) = function (get_state state) input
         in (r, set_state state s)
         end

      fun reconstr state (cmd as (t,c,l)) =
         let val reconstr = lifted Semantics.reconstr
             fun substitute (state as (sstate,exstate)) cmd = 
                   let val (r,sstate') =  Substitute.reconstr sstate cmd
(*                     val _ = print ("UnifySemantics.reconstr: Substitute.reconstr ("^t^"."^c^" "^(Induction.reprListToString l)^") returned: "^(Induction.reprToString r)^"\n") *)
                   in (r,(sstate',exstate))
                   end
         in case t of
                "substitute" => substitute state cmd
              | _ => if t = map_name
                        then case (c,l) of
                                 ("id",_) => reconstr state cmd
                               | _ => reconstr state cmd
                        else raise Fail (errmsg "reconstr" "no case" t c l)
         end

      fun deconstr state repr =
         let val (result as (cmd as (t,c,l),state')) = lifted Semantics.deconstr state repr
         in case cmd of 
               ("reprrepr","repr",_) => (Deconstructor.deconstr repr,state')
              | _ => (cmd,state')
         end
   end
end
