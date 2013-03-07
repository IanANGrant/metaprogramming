signature Infixop =
sig
   type infixop
   type exp
   val rep_comb : exp -> infixop -> infixop
   val rep_exp : exp -> infixop
   val prt_infixop : (exp -> 'a) ->
                     (exp -> infixop -> 'a) ->
                      infixop -> 'a
   val deconstruct_infixop : ('a -> 'b) -> ('c -> 'b) ->
                         (('c -> string * string * 'b list) ->
                          ('c -> 'a -> string * string * 'b list) -> 'd) -> 'd
   val construct_infixop :
                           ('a -> infixop) -> ('a -> exp) ->
                           (infixop -> 'b) ->
                            string * 'a list -> 'b
end

functor AbstractInfixop
  (type infixop
   type exp
   val rep_comb : exp -> infixop -> infixop
   val rep_exp : exp -> infixop
   val prt_infixop : (exp -> 'a) ->
                     (exp -> infixop -> 'a) ->
                      infixop -> 'a
  ) :> Infixop
  where type infixop = infixop
    and type exp = exp =
struct
   type infixop = infixop
   type exp = exp
   val rep_comb = rep_comb
   val rep_exp = rep_exp
   val prt_infixop = prt_infixop
   fun deconstruct_infixop infixop exp =
      fn f => f (fn e => ("infixop","exp", [exp e]))
                (fn e => fn es => ("infixop","comb",[exp e, infixop es]))
   fun construct_infixop infixop exp rep_infixop =
      fn arg => rep_infixop
           (case arg of ("exp",[e]) => rep_exp (exp e)
                      | ("comb",[e,es]) => rep_comb (exp e) (infixop es)
                      | (c,l) => raise Fail "infixop: no case")
end
