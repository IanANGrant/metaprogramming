signature Unit =
sig
   type repr
   val unit : repr -> repr
end

functor Unit
  (val module_name : string
   type repr
   structure Deconstructor : Deconstructor
          where type repr = repr
   structure Constructor : Constructor
          where type repr = repr
   structure Induction : Induction
          where type repr = repr
   ) :> Unit
          where type repr = repr =
struct
   local
      fun errmsg function part t s l =
          let val params = t^"."^s^","^(Induction.reprListToString l)
          in  module_name^"."^function^": "^part^": ("^params^")"
          end
      fun reconstr deconstructor output =
            fn state => fn (t,c,l) =>
              let val corecur = deconstructor output
                  fun iter r  [] = (rev r)
                    | iter r  (arg::args) =
                        let val e = corecur state arg
                        in iter (e::r) args
                        end
              in case (t,c,l) of ("str","string",[s]) => s (* Constructor.constr (t,c,l) *)
                     | _ => Constructor.constr (t,c,iter [] l)
              end
       val deconstruct = Induction.fromRep Deconstructor.deconstr
       val reconstruct = reconstr
   in
      type repr = repr
      val unit = Induction.induction deconstruct reconstruct () ()
   end
end

