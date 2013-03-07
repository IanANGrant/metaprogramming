signature Induction =
sig
   type repr
   type exp
   type infixop
   type bind
   type pat
   type atype
   eqtype str
   type reprrepr
   structure Exp : Exp where type exp = exp
                         and type infixop = infixop
                         and type bind = bind
                         and type atype = atype
                         and type str = str
   structure Infixop : Infixop where type infixop = infixop
                                 and type exp = exp
   structure Bind : Bind where type bind = bind
                           and type exp = exp
                           and type pat = pat 
                           and type atype = atype
                           and type str = str
   structure Pat : Pat  where type pat = pat
                          and type atype = atype
                          and type str = str 
   structure Atype : Atype  where type atype = atype
                              and type str = str
   structure Str : Str where type str = str
   structure ReprRepr : ReprRepr where type repr = repr
                                   and type reprrepr = reprrepr
   structure Repr : Repr where type repr = repr
                           and type exp = exp
                           and type infixop = infixop
                           and type bind = bind
                           and type pat = pat 
                           and type atype = atype
                           and type str = str
                           and type reprrepr = reprrepr

   val dec_repr_exp :
     (('a -> 'a) -> ('b -> 'c) -> ('d -> 'e) -> ('f -> 'g) -> ('h -> 'i) ->
      ('j -> 'k) -> ('l -> 'm) -> 'n) -> 'n
   val dec_repr_infixop :
     (('a -> 'b) -> ('c -> 'c) -> ('d -> 'e) -> ('f -> 'g) -> ('h -> 'i) ->
      ('j -> 'k) -> ('l -> 'm) -> 'n) -> 'n
   val dec_repr_bind :
     (('a -> 'b) -> ('c -> 'd) -> ('e -> 'e) -> ('f -> 'g) -> ('h -> 'i) ->
      ('j -> 'k) -> ('l -> 'm) -> 'n) -> 'n
   val dec_repr_pat :
     (('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'g) -> ('h -> 'i) ->
      ('j -> 'k) -> ('l -> 'm) -> 'n) -> 'n
   val dec_repr_atype :
     (('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> ('i -> 'i) ->
      ('j -> 'k) -> ('l -> 'm) -> 'n) -> 'n
   val dec_repr_str :
     (('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> ('i -> 'j) ->
      ('k -> 'k) -> ('l -> 'm) -> 'n) -> 'n
   val  dec_repr_repr :
     (('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> ('i -> 'j) ->
      ('k -> 'l) -> ('m -> 'm) -> 'n) -> 'n
   val deconstruct_rep :
       'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'h
   val deconstructor : 
     ((exp -> repr) -> (infixop -> repr) -> (bind -> repr) -> (atype -> repr) -> (str -> repr) ->
      ((infixop -> 'a) -> (bind -> 'a) -> (atype -> 'a) -> (str -> 'a) ->
       (exp -> exp -> 'a) -> (exp -> exp -> 'a) -> exp -> 'a) -> exp -> 'b) ->
     ((infixop -> repr) -> (exp -> repr) ->
      ((exp -> 'c) -> (exp -> infixop -> 'c) -> infixop -> 'c) -> infixop -> 'b) ->
     ((exp -> repr) -> (pat -> repr) -> (str -> repr) ->
     ((pat -> exp -> 'd) -> (str -> exp -> 'd) -> 
      (pat -> exp -> exp -> 'd) -> bind -> 'd) -> bind -> 'b) ->
     ((pat -> repr) -> (atype -> repr) -> (str -> repr) ->
      ((unit -> 'e) -> (atype -> 'e) -> (str -> atype -> 'e) ->
       (pat -> pat -> 'e) -> (str -> pat -> 'e) -> pat -> 'e) -> pat -> 'b) ->
     ((atype -> repr) -> (str -> repr) ->
      ((str -> 'f) -> (str -> atype -> 'f) ->
       (atype -> atype -> 'f) -> atype -> 'f) -> atype -> 'b) ->
     ((str -> repr) -> ((str -> 'g) -> str -> 'g) -> str -> 'b) ->
     (((unit -> 'h) -> (repr -> repr -> 'h) -> reprrepr -> 'h) -> reprrepr -> 'b) ->
      repr -> 'b

   val constructor: 
     ((repr -> exp) -> (repr -> infixop) ->
      (repr -> bind) -> (repr -> atype) -> (repr -> str) -> (exp -> repr) ->
       string * repr list -> 'a) ->
     ((repr -> infixop) -> (repr -> exp) ->
      (infixop -> repr) -> string * repr list -> 'a) ->
     ((repr -> exp) -> (repr -> pat) ->
      (repr -> str) -> (bind -> repr) -> string * repr list -> 'a) ->
     ((repr -> pat) -> (repr -> atype) ->
      (repr -> str) -> (pat -> repr) -> string * repr list -> 'a) ->
     ((repr -> atype) -> (repr -> str) ->
      (atype -> repr) -> string * repr list -> 'a) ->
     ((repr -> str) -> (str -> repr) ->
      string * repr list -> 'a) ->
     ((reprrepr -> repr) ->
      string * repr list -> 'a) -> string * string * repr list -> 'a

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
   type exp
   type infixop
   type bind
   type pat
   type atype
   eqtype str
   type repr
   type reprrepr
   structure Exp : Exp where type exp = exp
                         and type infixop = infixop
                         and type bind = bind
                         and type atype = atype
                         and type str = str
   structure Infixop : Infixop where type infixop = infixop
                                 and type exp = exp
   structure Bind : Bind where type bind = bind
                           and type exp = exp
                           and type pat = pat 
                           and type atype = atype
                           and type str = str
   structure Pat : Pat where type pat = pat
                         and type atype = atype
                         and type str = str 
   structure Atype : Atype where type atype = atype
                             and type str = str
   structure Str : Str  where type str = str
   structure ReprRepr : ReprRepr where type repr = repr
                                   and type reprrepr = reprrepr
   structure Repr : Repr where type repr = repr
                           and type exp = exp
                           and type infixop = infixop
                           and type bind = bind
                           and type pat = pat 
                           and type atype = atype
                           and type str = str
                           and type reprrepr = reprrepr
  ) :> Induction
    where type exp = exp
      and type infixop = infixop
      and type bind = bind
      and type pat = pat
      and type atype = atype
      and type str = str 
      and type repr = repr
      and type reprrepr = reprrepr =
struct
   type exp = exp
   type infixop = infixop
   type bind = bind
   type pat = pat
   type atype = atype
   type str = str
   type repr = repr
   type reprrepr = reprrepr
   structure Exp :> Exp where type exp = exp
                         and type infixop = infixop
                         and type bind = bind
                         and type atype = atype
                         and type str = str = Exp
   structure Infixop :> Infixop where type infixop = infixop
                                 and type exp = exp = Infixop
   structure Bind :> Bind where type bind = bind
                           and type exp = exp
                           and type pat = pat
                           and type atype = atype
                           and type str = str = Bind
   structure Pat :> Pat  where type pat = pat
                          and type atype = atype
                          and type str = str = Pat
   structure Atype :> Atype  where type atype = atype
                              and type str = str = Atype
   structure Str :> Str  where type str = str = Str
   structure ReprRepr :> ReprRepr where type repr = repr
                                   and type reprrepr = reprrepr = ReprRepr
   structure Repr :> Repr where type repr = repr
                           and type exp = exp
                           and type infixop = infixop
                           and type bind = bind
                           and type pat = pat 
                           and type atype = atype
                           and type str = str
                           and type reprrepr = reprrepr = Repr

   fun dec_repr_exp f =
     let fun excn s = Fail ("dec_repr_exp: "^s)
     in f (fn b => b)
          (fn _ => raise (excn "infixop"))
          (fn _ => raise (excn "bind"))
          (fn _ => raise (excn "pat"))
          (fn _ => raise (excn "atype"))
          (fn _ => raise (excn "str"))
          (fn _ => raise (excn "repr"))
     end

   fun dec_repr_infixop f =
     let fun excn s = Fail ("dec_repr_infixop: "^s)
     in f (fn _ => raise (excn "exp"))
          (fn b => b)
          (fn _ => raise (excn "bind"))
          (fn _ => raise (excn "pat"))
          (fn _ => raise (excn "atype"))
          (fn _ => raise (excn "str"))
          (fn _ => raise (excn "repr"))
     end

   fun dec_repr_bind f =
     let fun excn s = Fail ("dec_repr_bind: "^s)
     in f (fn _ => raise (excn "exp"))
          (fn _ => raise (excn "infixop"))
          (fn b => b)
          (fn _ => raise (excn "pat"))
          (fn _ => raise (excn "atype"))
          (fn _ => raise (excn "str"))
          (fn _ => raise (excn "repr"))
     end

   fun dec_repr_pat f =
     let fun excn s = Fail ("dec_repr_pat: "^s)
     in f (fn _ => raise (excn "exp"))
          (fn _ => raise (excn "infixop"))
          (fn _ => raise (excn "bind"))
          (fn b => b)
          (fn _ => raise (excn "atype"))
          (fn _ => raise (excn "str"))
          (fn _ => raise (excn "repr"))
     end

   fun dec_repr_atype f =
     let fun excn s = Fail ("dec_repr_atype: "^s)
     in f (fn _ => raise (excn "exp"))
          (fn _ => raise (excn "infixop"))
          (fn _ => raise (excn "bind"))
          (fn _ => raise (excn "pat"))
          (fn b => b)
          (fn _ => raise (excn "str"))
          (fn _ => raise (excn "repr"))
     end

   fun dec_repr_str f =
     let fun excn s = Fail ("dec_repr_str: "^s)
     in f (fn _ => raise (excn "exp"))
          (fn _ => raise (excn "infixop"))
          (fn _ => raise (excn "bind"))
          (fn _ => raise (excn "pat"))
          (fn _ => raise (excn "atype"))
          (fn b => b)
          (fn _ => raise (excn "repr"))
     end

   fun dec_repr_repr f =
     let fun excn s = Fail ("dec_repr_repr: "^s)
     in f (fn _ => raise (excn "exp"))
          (fn _ => raise (excn "infixop"))
          (fn _ => raise (excn "bind"))
          (fn _ => raise (excn "pat"))
          (fn _ => raise (excn "atype"))
          (fn _ => raise (excn "str"))
          (fn b => b)
     end

   fun deconstruct_rep exp infixop bind pat atype str reprrepr =
              fn f => f exp infixop bind pat atype str reprrepr

   fun deconstructor exp infixop bind pat atype str reprrepr =
      let open Repr 
      in
         deconstruct_rep
            (exp     rep_exp rep_infixop rep_bind rep_atype rep_str     Exp.prt_exp)
            (infixop rep_infixop rep_exp                            Infixop.prt_infixop)
            (bind    rep_exp rep_pat rep_str                           Bind.prt_bind)
            (pat     rep_pat rep_atype rep_str                          Pat.prt_pat)
            (atype   rep_atype rep_str                                Atype.prt_atype)
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
                       Exp.deconstruct_exp
                   Infixop.deconstruct_infixop
                      Bind.deconstruct_bind
                       Pat.deconstruct_pat
                     Atype.deconstruct_atype
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
         let val prt_exp     = dec_repr_exp     Repr.prt_repr
             val prt_infixop = dec_repr_infixop Repr.prt_repr
             val prt_bind    = dec_repr_bind    Repr.prt_repr
             val prt_pat     = dec_repr_pat     Repr.prt_repr
             val prt_atype   = dec_repr_atype   Repr.prt_repr
             val prt_str     = dec_repr_str     Repr.prt_repr
         in case t of
                "exp" => exp prt_exp prt_infixop prt_bind prt_atype prt_str     Repr.rep_exp (c,l)
              | "infixop" => infixop prt_infixop prt_exp                    Repr.rep_infixop (c,l)
              | "bind" => bind prt_exp prt_pat prt_str                         Repr.rep_bind (c,l)
              | "pat" => pat prt_pat prt_atype prt_str                          Repr.rep_pat (c,l)
              | "atype"=> atype prt_atype prt_str                             Repr.rep_atype (c,l)
              | "str" => str prt_str                                            Repr.rep_str (c,l)
              | "repr" => repr                                             Repr.rep_reprrepr (c,l)
              | _ => raise Fail ("no type "^t)
         end handle Fail s => 
                       raise Fail (module_name^".constructor: "^s^": ("^
                                    t^"."^c^" "^(reprListToString l)^")")
   end
end
