signature ReprEmbedding =
sig
   val module_name : string
   type repr
   type reprrepr
   eqtype str
   val reprToString : repr -> string
   val reprListToString : repr list -> string
   structure Deconstructor : Deconstructor
      where type repr = repr
   structure Constructor : Constructor
      where type repr = repr
end

functor ReprEmbedding
  (val module_name : string
   eqtype str
   type repr
   type reprrepr
   structure Induction : Induction
       where type str = str
         and type repr = repr
         and type reprrepr = reprrepr
   ) :> ReprEmbedding
       where type str = str
         and type repr = repr
         and type reprrepr = reprrepr =
struct
   val module_name = module_name

   type str = str
   type repr = repr
   type reprrepr = reprrepr

   local
      val fromMLString = Induction.Str.fromMLString
      val toMLString = Induction.Str.toMLString
      val rep_str = Induction.Repr.rep_str
      val str = Induction.dec_repr_str Induction.Repr.prt_repr
      val r_rr = Induction.dec_repr_repr Induction.Repr.prt_repr
      val prt_reprrepr = Induction.ReprRepr.prt_reprrepr
      val reprnil = Induction.Repr.rep_reprrepr (Induction.ReprRepr.rep_nil ())
      fun reprcons a l = Induction.Repr.rep_reprrepr (Induction.ReprRepr.rep_repr a l)

      fun foldright f =
          let val rec iter = fn r => Induction.ReprRepr.prt_reprrepr
                 (fn () => r)
                 (fn a => fn b => iter (f a r) (r_rr b))
          in iter
          end

      fun foldleft f =
          let val rec iter = fn r => Induction.ReprRepr.prt_reprrepr
                  (fn () => r)
                    (fn a => fn b => f a (iter r (r_rr b)))
          in iter
          end

      val reverse = foldright reprcons reprnil

      fun reprListToRepr [] = reprnil
        | reprListToRepr (v::l) = reprcons v (reprListToRepr l)

      fun reprToList rs =
         rev (foldright (fn a => fn r => a::r)
                   [] (r_rr rs))

      fun mk_reprrepr (t,c,l) = reprcons (reprcons (rep_str (fromMLString t))
                                                   (rep_str (fromMLString c)))
                                                   (reprListToRepr l)

      fun dst_reprrepr r =
          let val (tc,lr) = 
                 Induction.ReprRepr.prt_reprrepr
                    (fn () => raise Fail (module_name^".extract_repr: nil pair"))
                    (fn a => fn b => (a,b)) (r_rr r)
              val (t,c) = 
                 Induction.ReprRepr.prt_reprrepr
                    (fn () => raise Fail (module_name^".extract_repr: nil pair"))
                    (fn a => fn b => (a,b)) (r_rr tc)
          in (toMLString (str t),toMLString (str c),reprToList lr)
          end

      fun toString str deconstructor =
         fn output => fn state => fn (t,cn,args) => 
              let val corecur = deconstructor output
                  val state = output state (t^"."^cn)
                  fun iter state [] = state
                    | iter state (arg::args) =
                        let fun stringarg arg = output state (" '"^(str arg)^"'")
                            fun otherarg arg = output (corecur (output state " (") arg) ")"
                            (* val _ = print ("ReprEmbedding.toStr: "^t^"."^cn^" "^
                                            (Induction.reprListToString (arg::args))^"\n") *)
                            val state = case (t,cn) of
                                          ("str","string") =>
                                              (case dst_reprrepr arg of 
                                                  ("str","string",[arg]) => stringarg arg
                                                | _ => raise Fail ("ReprEmbedding.reprToString: reprrepr"))
                                        | ("bindvar","var") => stringarg arg
                                        | ("atyvar","var") => stringarg arg
                                        | ("exp","var") => stringarg arg
                                        | ("bindvar","meta") => stringarg arg
                                        | ("atyvar","meta") => stringarg arg
                                        | ("exp","meta") => stringarg arg
                                        | ("monomial","name") => stringarg arg
                                        | _ => otherarg arg
                        in iter state args
                        end
              in iter state args
              end

      fun stringPrinter state = fn s => state^s

      val toStr = Induction.Str.toMLString o (Induction.dec_repr_str Induction.Repr.prt_repr)
   in
      structure Deconstructor :> Deconstructor where type repr = repr =
      struct
          type repr = repr
          val deconstr = dst_reprrepr
      end
      structure Constructor :> Constructor where type repr = repr =
      struct
          type repr = repr
          val constr = mk_reprrepr
      end
      val reprToString = Induction.induction (Induction.fromRep Deconstructor.deconstr)
                                    (toString toStr) stringPrinter ""

      fun reprListToString l =
         let fun iter s [] = "["^s^"]"
               | iter s (r::rs) =
                     let val sep = if s = "" then "" else ", " 
                     in iter (s^sep^(reprToString r)) rs
                     end
         in iter "" l
         end
   end
end
