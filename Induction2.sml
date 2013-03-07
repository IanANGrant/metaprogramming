signature Induction =
sig
   type repr
   eqtype str
   type reprrepr
   structure Str : Str where type str = str
   structure ReprRepr : ReprRepr where type repr = repr
                                   and type reprrepr = reprrepr
   structure Repr : Repr2 where type repr = repr
                           and type str = str
                           and type reprrepr = reprrepr

   val  dec_repr_str : (('a -> 'a) -> ('b -> 'c) -> 'd) -> 'd
   val  dec_repr_repr : (('a -> 'b) -> ('c -> 'c) -> 'd) -> 'd
   val deconstruct_rep : 'a -> 'b -> ('a -> 'b -> 'c) -> 'c
   val deconstructor : 
         ((str -> repr) -> ((str -> 'a) -> str -> 'a) -> str -> 'b) ->
         (((unit -> 'c) -> (repr -> repr -> 'c) -> reprrepr -> 'c) -> reprrepr ->
          'b) -> repr -> 'b

   val constructor: 
         'a -> 'b -> 'c -> 'd -> 'e ->
         ((repr -> str) -> (str -> repr) -> string * repr list -> 'f) ->
         ((reprrepr -> repr) -> string * repr list -> 'f) ->
           string * string * repr list -> 'f

   val fromRep : ('a -> 'b) -> ('c -> 'd -> 'b -> 'e) -> 'c -> 'd -> 'a -> 'e
   val induction : (('a -> 'b -> 'c) -> 'd -> 'e -> 'f) -> 
                   (('d -> 'e -> 'f) -> 'a -> 'b -> 'c) ->
                     'd -> 'e -> 'f
   val exduction : (('a -> 'b -> 'c) -> 'd -> 'e -> 'f) -> 
                   (('d -> 'e -> 'f) -> 'a -> 'b -> 'c) ->
                     'a -> 'b -> 'c
   val reprToString : repr -> string
   val reprListToString : repr list -> string
end

functor Induction
  (val module_name : string
   eqtype str
   type repr
   type reprrepr
   structure Str : Str  where type str = str
   structure ReprRepr : ReprRepr where type repr = repr
                                   and type reprrepr = reprrepr
   structure Repr : Repr2 where type repr = repr
                           and type str = str
                           and type reprrepr = reprrepr
  ) :> Induction
    where type str = str 
      and type repr = repr
      and type reprrepr = reprrepr =
struct
   type str = str
   type repr = repr
   type reprrepr = reprrepr
   structure Str :> Str  where type str = str = Str
   structure ReprRepr :> ReprRepr where type repr = repr
                                   and type reprrepr = reprrepr = ReprRepr
   structure Repr :> Repr2 where type repr = repr
                           and type str = str
                           and type reprrepr = reprrepr = Repr

   fun dec_repr_str f =
     let fun excn s = Fail ("dec_repr_str: "^s)
     in f (fn b => b)
          (fn _ => raise (excn "repr"))
     end

   fun dec_repr_repr f =
     let fun excn s = Fail ("dec_repr_repr: "^s)
     in f (fn _ => raise (excn "str"))
          (fn b => b)
     end

   fun deconstruct_rep str reprrepr =
              fn f => f str reprrepr

   fun deconstructor str reprrepr =
      let open Repr 
      in
         deconstruct_rep
            (str     rep_str                                            Str.prt_string)
            (reprrepr                                              ReprRepr.prt_reprrepr)
             Repr.prt_repr
      end

   fun fromRep deconstruct reconstruct =
      fn argl => fn argr => fn input => 
              reconstruct argl argr (deconstruct input)

   fun induction deconstruct reconstruct =
      let fun reconstructor argl = fn argr =>
                     reconstruct deconstructor argl argr
           and deconstructor argl = fn argr =>
                     deconstruct reconstructor argl argr
      in
         deconstructor
      end

   fun exduction deconstruct reconstruct =
      let fun reconstructor argl = fn argr =>
                     reconstruct deconstructor argl argr
           and deconstructor argl = fn argr =>
                     deconstruct reconstructor argl argr
      in
         reconstructor
      end

   local
      val deconstr = deconstructor
                       Str.deconstruct_str
                  ReprRepr.deconstruct_reprrepr

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

      fun stringPrinter state = fn s => state^s

      val toStr = Str.toMLString o (dec_repr_str Repr.prt_repr)
   in
      val reprToString = induction (fromRep deconstr) (toString toStr) stringPrinter ""

      fun reprListToString l =
         let fun iter s [] = "["^s^"]"
               | iter s (r::rs) =
                     let val sep = if s = "" then "" else ", " 
                     in iter (s^sep^(reprToString r)) rs
                     end
         in iter "" l
         end

      fun constructor exp infixop bind pat atype str repr (t:string,c:string,l:repr list) =
         let val prt_str     = dec_repr_str     Repr.prt_repr
         in case t of
                "str" => str prt_str                                            Repr.rep_str (c,l)
              | "repr" => repr                                             Repr.rep_reprrepr (c,l)
              | _ => raise Fail ("no type "^t)
         end handle Fail s => 
                       raise Fail (module_name^".constructor: "^s^": ("^
                                    t^"."^c^" "^(reprListToString l)^")")
   end
end
