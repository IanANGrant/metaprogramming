signature Deconstructor =
sig
    type repr
    val deconstr : repr -> string * string * repr list
end

functor Deconstructor
  (type exp
   type infixop
   type bind
   type pat
   type atype
   eqtype str
   type reprrepr
   type repr
   val prt_exp : (infixop -> 'a) ->
                 (bind -> 'a) ->
                 (atype -> 'a) ->
                 (str -> 'a) ->
                 (exp -> exp -> 'a) ->
                 (exp -> exp -> 'a) ->
                  exp -> 'a
   val deconstruct_exp :
              ('a -> 'b) -> ('c -> 'b) -> ('d -> 'b) -> ('e -> 'b) -> ('f -> 'b) ->
             (('c -> string * string * 'b list) -> ('d -> string * string * 'b list) ->
              ('e -> string * string * 'b list) -> ('f -> string * string * 'b list) ->
              ('a -> 'a -> string * string * 'b list) ->
              ('a -> 'a -> string * string * 'b list) -> 'g) -> 'g
   val prt_infixop : (exp -> 'a) ->
                     (exp -> infixop -> 'a) ->
                      infixop -> 'a
   val deconstruct_infixop : ('a -> 'b) -> ('c -> 'b) ->
                         (('c -> string * string * 'b list) ->
                          ('c -> 'a -> string * string * 'b list) -> 'd) -> 'd
   val prt_bind : (pat -> exp -> 'a) ->
                  (str -> exp -> 'a) ->
                  (pat -> exp -> exp -> 'a) -> 
                   bind -> 'a
   val deconstruct_bind : ('a -> 'b) -> ('c -> 'b) -> ('e -> 'b) ->
                         (('c -> 'a -> string * string * 'b list) ->
                          ('e -> 'a -> string * string * 'b list) ->
                          ('c -> 'a -> 'a -> string * string * 'b list) -> 'f) -> 'f
   val prt_pat : (unit -> 'a) -> (atype -> 'a) -> (str -> atype -> 'a) ->
                 (pat -> pat -> 'a) -> (str -> pat -> 'a) -> pat -> 'a
   val deconstruct_pat : ('a -> 'b) -> ('c -> 'b) -> ('d -> 'b) ->
         ((unit -> string * string * 'e list) -> ('c -> string * string * 'b list) ->
          ('d -> 'c -> string * string * 'b list) ->
          ('a -> 'a -> string * string * 'b list) ->
          ('d -> 'a -> string * string * 'b list) -> 'f) -> 'f
   val prt_atype : (str -> 'a) ->
                   (str -> atype -> 'a) ->
                   (atype -> atype -> 'a) ->
                    atype -> 'a
   val deconstruct_atype : ('a -> 'b) -> ('c -> 'b) ->
                          (('c -> string * string * 'b list) ->
                           ('c -> 'a -> string * string * 'b list) ->
                           ('a -> 'a -> string * string * 'b list) -> 'd) -> 'd
   val prt_string : (str -> 'a) -> str -> 'a
   val deconstruct_str : ('a -> 'b) -> (('a -> string * string * 'b list) -> 'c) -> 'c
   val prt_reprrepr : (unit -> 'a) -> (repr -> repr -> 'a) -> reprrepr -> 'a 
   val deconstruct_reprrepr : ((unit -> string * string * 'b list) -> 
                               ('a -> 'a -> string * string * 'a list) ->'c) -> 'c
   structure Induction : Induction
       where type exp = exp
         and type infixop = infixop
         and type bind = bind
         and type pat = pat
         and type atype = atype
         and type str = str
         and type reprrepr = reprrepr
         and type repr = repr
         ) :> Deconstructor
  where type repr = repr =
struct
   type exp = exp
   type infixop = infixop
   type bind = bind
   type pat = pat
   type atype = atype
   type str = str
   type repr = repr
   type reprrepr = reprrepr
   val deconstr =
       let open Induction 
       in Induction.deconstruct_rep
            (deconstruct_exp  Repr.rep_exp Repr.rep_infixop
                              Repr.rep_bind Repr.rep_atype Repr.rep_str  prt_exp)
            (deconstruct_infixop Repr.rep_infixop Repr.rep_exp           prt_infixop)
            (deconstruct_bind  Repr.rep_exp Repr.rep_pat Repr.rep_str    prt_bind)
            (deconstruct_pat   Repr.rep_pat Repr.rep_atype Repr.rep_str  prt_pat)
            (deconstruct_atype Repr.rep_atype Repr.rep_str               prt_atype)
            (deconstruct_str   Repr.rep_str                              prt_string)
            (deconstruct_reprrepr                                        prt_reprrepr)
             Repr.prt_repr
       end
end
