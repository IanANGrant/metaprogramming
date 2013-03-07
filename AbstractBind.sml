signature Bind =
sig
   type bind
   type exp
   type atype
   type pat
   type str
   val rep_abs : pat -> exp -> bind
   val rep_let : pat -> exp -> exp -> bind
   val rep_lambda : str -> exp -> bind
   val prt_bind : (pat -> exp -> 'a) ->
                  (str -> exp -> 'a) ->
                  (pat -> exp -> exp -> 'a) -> 
                   bind -> 'a
   val deconstruct_bind : ('a -> 'b) -> ('c -> 'b) -> ('e -> 'b) ->
                         (('c -> 'a -> string * string * 'b list) ->
                          ('e -> 'a -> string * string * 'b list) ->
                          ('c -> 'a -> 'a -> string * string * 'b list) -> 'f) -> 'f
   val construct_bind :
                        ('a -> exp) -> ('a -> pat) -> ('a -> str) -> (bind -> 'b) ->
                         string * 'a list -> 'b
end

functor AbstractBind
  (type bind
   type exp
   type atype
   type pat
   eqtype str
   val rep_abs : pat -> exp -> bind
   val rep_let : pat -> exp -> exp -> bind
   val rep_lambda : str -> exp -> bind
   val prt_bind : (pat -> exp -> 'a) ->
                  (str -> exp -> 'a) ->
                  (pat -> exp -> exp -> 'a) -> 
                   bind -> 'a
  ) :> Bind
  where type bind = bind
    and type exp = exp 
    and type atype = atype
    and type pat = pat
    and type str = str =
struct
   type bind = bind
   type atype = atype
   type exp = exp
   type pat = pat
   type str = str
   val rep_abs = rep_abs
   val rep_let = rep_let
   val rep_lambda = rep_lambda
   val prt_bind = prt_bind
   fun deconstruct_bind exp pat str =
      fn f => f (fn p => fn e => ("bind","abs", [pat p, exp e]))
                (fn v => fn e => ("bind","lambda",[str v, exp e]))
                (fn p => fn e => fn e' => ("bind","let",[pat p, exp e, exp e']))
   fun construct_bind exp pat str rep_bind =
      fn arg => rep_bind
           (case arg of ("abs",[p,e]) => rep_abs (pat p) (exp e)
                      | ("let",[p,e,e']) => rep_let (pat p) (exp e) (exp e')
                      | ("lambda",[v,e]) => rep_lambda (str v) (exp e)
                      | (c,l) => raise Fail "bind: no case")
end
