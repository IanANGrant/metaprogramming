signature Exp =
sig
   type exp
   type infixop
   type bind
   type atype
   eqtype str
   val rep_infixopexp : infixop -> exp
   val rep_bindexp : bind -> exp
   val rep_var : str -> exp
   val rep_atypeexp : atype -> exp
   val rep_comb : exp -> exp -> exp
   val rep_pairexp : exp -> exp -> exp
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
   val construct_exp : 
                       ('a -> exp) -> ('a -> infixop) -> ('a -> bind) ->
                       ('a -> atype) -> ('a -> str) ->
                       (exp -> 'b) -> string * 'a list -> 'b
end

functor AbstractExp
   (type exp
    type infixop
    type bind
    type atype
    eqtype str
    val rep_infixopexp : infixop -> exp
    val rep_bindexp : bind -> exp
    val rep_var : str -> exp
    val rep_atypeexp : atype -> exp
    val rep_comb : exp -> exp -> exp
    val rep_pairexp : exp -> exp -> exp
    val prt_exp : (infixop -> 'a) ->
                  (bind -> 'a) ->
                  (atype -> 'a) ->
                  (str -> 'a) ->
                  (exp -> exp -> 'a) ->
                  (exp -> exp -> 'a) ->
                   exp -> 'a
   ) :> Exp
  where type exp = exp
    and type infixop = infixop
    and type bind = bind
    and type atype = atype
    and type str = str =
struct
   type infixop = infixop
   type bind = bind
   type atype = atype
   type str = str
   type exp = exp
   val rep_infixopexp = rep_infixopexp
   val rep_bindexp = rep_bindexp
   val rep_var = rep_var
   val rep_atypeexp = rep_atypeexp
   val rep_comb = rep_comb
   val rep_pairexp = rep_pairexp
   val prt_exp = prt_exp
   fun deconstruct_exp (exp:'a->'b) (infixop:'c->'b) (bind:'d->'b)
                                       (atype:'e->'b) (str:'f->'b) =
      fn f => f (fn i => ("exp","infixopexp", [infixop i]))
                (fn b => ("exp","bindexp", [bind b]))
                (fn t => ("exp","atypeexp",[atype t]))
                (fn v => ("exp","var",[str v]))
                (fn e => fn e' => ("exp","comb", [exp e, exp e']))
                (fn e => fn e' => ("exp","pairexp", [exp e, exp e']))
   fun construct_exp exp infixop bind atype str rep_exp =
      fn arg => rep_exp
           (case arg of
                ("infixopexp",[i]) => rep_infixopexp (infixop i)
              | ("bindexp",[b]) => rep_bindexp (bind b)
              | ("atypeexp",[t]) => rep_atypeexp (atype t)
              | ("var",[v]) => rep_var (str v)
              | ("comb",[e,e']) => rep_comb (exp e) (exp e')
              | ("pairexp",[e,e']) => rep_pairexp (exp e) (exp e')
              | (c,l) => raise Fail "exp: no case")
end
