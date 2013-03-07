signature StringPrinter =
sig
    type repr
    val reprToString : repr -> string
    val reprListToString : repr list -> string
end

functor StringPrinter
  (type repr
   structure Deconstructor : Deconstructor where type repr = repr
   structure Induction : Induction where type repr = repr) :> StringPrinter
     where type repr = repr =
struct
   type repr = repr
   local
      fun toString str deconstructor =
         fn output => fn state => fn (t,cn,args) => 
              let val corecur = deconstructor output
                  val state = output state (t^"."^cn)
                  fun iter state [] = state
                    | iter state (arg::args) =
                        let fun stringarg arg = output state (" '"^(str arg)^"'")
                            fun otherarg arg = output (corecur (output state " (") arg) ")"
                            val state = case (t,cn) of
                                          ("str","string") => stringarg arg
                                        | _ => otherarg arg
                        in iter state args
                        end
              in iter state args
              end

      open Induction 
      fun stringPrinter state = fn s => state^s
      val toStr = Str.toMLString o (dec_repr_str Repr.prt_repr)
   in
      val deconstructor = Deconstructor.deconstr
      val reprToString = induction (fromRep deconstructor) (toString toStr) stringPrinter ""
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
