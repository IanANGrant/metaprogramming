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

   val constructor: (string -> string -> repr list -> string) ->
     ((string -> repr list -> exn) -> (repr -> exp) -> (repr -> infixop) ->
      (repr -> bind) -> (repr -> atype) -> (repr -> str) -> (exp -> repr) ->
       string * repr list -> 'a) ->
     ((string -> repr list -> exn) -> (repr -> infixop) -> (repr -> exp) ->
      (infixop -> repr) -> string * repr list -> 'a) ->
     ((string -> repr list -> exn) -> (repr -> exp) -> (repr -> pat) ->
      (repr -> str) -> (bind -> repr) -> string * repr list -> 'a) ->
     ((string -> repr list -> exn) -> (repr -> pat) -> (repr -> atype) ->
      (repr -> str) -> (pat -> repr) -> string * repr list -> 'a) ->
     ((string -> repr list -> exn) -> (repr -> atype) -> (repr -> str) ->
      (atype -> repr) -> string * repr list -> 'a) ->
     ((string -> repr list -> exn) -> (repr -> str) -> (str -> repr) ->
      string * repr list -> 'a) ->
     ((string -> repr list -> exn) -> (reprrepr -> repr) ->
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
  (type exp
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
   structure Exp : Exp where type exp = exp
                         and type infixop = infixop
                         and type bind = bind
                         and type atype = atype
                         and type str = str = Exp
   structure Infixop : Infixop where type infixop = infixop
                                 and type exp = exp = Infixop
   structure Bind : Bind where type bind = bind
                           and type exp = exp
                           and type pat = pat
                           and type atype = atype
                           and type str = str = Bind
   structure Pat : Pat  where type pat = pat
                          and type atype = atype
                          and type str = str = Pat
   structure Atype : Atype  where type atype = atype
                              and type str = str = Atype
   structure Str : Str  where type str = str = Str
   structure ReprRepr : ReprRepr where type repr = repr
                                   and type reprrepr = reprrepr = ReprRepr
   structure Repr : Repr where type repr = repr
                           and type exp = exp
                           and type infixop = infixop
                           and type bind = bind
                           and type pat = pat 
                           and type atype = atype
                           and type str = str
                           and type reprrepr = reprrepr = Repr

   fun dec_repr_exp f =
      f (fn b => b)
        (fn _ => raise Fail "dec_repr_exp: infixop")
        (fn _ => raise Fail "dec_repr_exp: bind")
        (fn _ => raise Fail "dec_repr_exp: pat")
        (fn _ => raise Fail "dec_repr_exp: atype")
        (fn _ => raise Fail "dec_repr_exp: str")
        (fn _ => raise Fail "dec_repr_exp: repr")

   fun dec_repr_infixop f =
      f (fn _ => raise Fail "dec_repr_infixop: exp")
        (fn b => b)
        (fn _ => raise Fail "dec_repr_infixop: bind")
        (fn _ => raise Fail "dec_repr_infixop: pat")
        (fn _ => raise Fail "dec_repr_infixop: atype")
        (fn _ => raise Fail "dec_repr_infixop: str")
        (fn _ => raise Fail "dec_repr_infixop: repr")

   fun dec_repr_bind f =
      f (fn _ => raise Fail "dec_repr_bind: exp")
        (fn _ => raise Fail "dec_repr_bind: infixop")
        (fn b => b)
        (fn _ => raise Fail "dec_repr_bind: pat")
        (fn _ => raise Fail "dec_repr_bind: atype")
        (fn _ => raise Fail "dec_repr_bind: str")
        (fn _ => raise Fail "dec_repr_bind: repr")

   fun dec_repr_pat f =
      f (fn _ => raise Fail "dec_repr_pat: exp")
        (fn _ => raise Fail "dec_repr_pat: infixop")
        (fn _ => raise Fail "dec_repr_pat: bind")
        (fn b => b)
        (fn _ => raise Fail "dec_repr_pat: atype")
        (fn _ => raise Fail "dec_repr_pat: str")
        (fn _ => raise Fail "dec_repr_pat: repr")

   fun dec_repr_atype f =
      f (fn _ => raise Fail "dec_repr_atype: exp")
        (fn _ => raise Fail "dec_repr_atype: infixop")
        (fn _ => raise Fail "dec_repr_atype: bind")
        (fn _ => raise Fail "dec_repr_atype: pat")
        (fn b => b)
        (fn _ => raise Fail "dec_repr_atype: str")
        (fn _ => raise Fail "dec_repr_atype: repr")

   fun dec_repr_str f =
      f (fn _ => raise Fail "dec_repr_str: exp")
        (fn _ => raise Fail "dec_repr_str: infixop")
        (fn _ => raise Fail "dec_repr_str: bind")
        (fn _ => raise Fail "dec_repr_str: pat")
        (fn _ => raise Fail "dec_repr_str: atype")
        (fn b => b)
        (fn _ => raise Fail "dec_repr_str: repr")

   fun dec_repr_repr f =
      f (fn _ => raise Fail "dec_repr_repr: exp")
        (fn _ => raise Fail "dec_repr_repr: infixop")
        (fn _ => raise Fail "dec_repr_repr: bind")
        (fn _ => raise Fail "dec_repr_repr: pat")
        (fn _ => raise Fail "dec_repr_repr: atype")
        (fn _ => raise Fail "dec_repr_repr: str")
        (fn b => b)

   fun deconstruct_rep exp infixop bind pat atype str repr =
              fn f => f exp infixop bind pat atype str repr

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

   fun constructor errmsg exp infixop bind pat atype str repr (t:string,c:string,l:repr list) =
      let fun err_excn (c:string) (l:repr list) = Fail (errmsg t c l)
      val prt_exp     = dec_repr_exp     Repr.prt_repr
      val prt_infixop = dec_repr_infixop Repr.prt_repr
      val prt_bind    = dec_repr_bind    Repr.prt_repr
      val prt_pat     = dec_repr_pat     Repr.prt_repr
      val prt_atype   = dec_repr_atype   Repr.prt_repr
      val prt_str     = dec_repr_str     Repr.prt_repr
      in case t of
          "exp" => exp err_excn prt_exp prt_infixop prt_bind prt_atype prt_str Repr.rep_exp (c,l)
        | "infixop" => infixop err_excn prt_infixop prt_exp                Repr.rep_infixop (c,l)
        | "bind" => bind err_excn prt_exp prt_pat prt_str                     Repr.rep_bind (c,l)
        | "pat" => pat err_excn prt_pat prt_atype prt_str                      Repr.rep_pat (c,l)
        | "atype"=> atype err_excn prt_atype prt_str                         Repr.rep_atype (c,l)
        | "str" => str err_excn prt_str                                        Repr.rep_str (c,l)
        | "repr" => repr err_excn                                         Repr.rep_reprrepr (c,l)
        | _ => raise Fail ("constructor: no type "^t)
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
   end
end
