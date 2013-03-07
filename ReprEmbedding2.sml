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

functor ReprEmbedding2
  (val module_name : string
   eqtype str
   type repr
   type reprrepr
   structure Constructor : Constructor
       where type repr = repr
   structure Deconstructor : Deconstructor
       where type repr = repr
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
      val rep_str = fn s => Constructor.constr ("str","string",[Induction.Repr.rep_str s])
      val str = Induction.dec_repr_str Induction.Repr.prt_repr
      val r_rr = Induction.dec_repr_repr Induction.Repr.prt_repr
      val prt_reprrepr = Induction.ReprRepr.prt_reprrepr
      val reprnil = Constructor.constr ("repr","nil",[])
      fun reprcons a l = Constructor.constr ("repr","repr",[a,l])

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
                        let val state = case Deconstructor.deconstr arg of
                                           ("str","string",[s]) => output state (" '"^(str arg)^"'")
                                         | _ => output (corecur (output state " (") arg) ")"
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
