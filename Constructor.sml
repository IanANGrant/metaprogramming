signature Constructor =
sig
    type repr
    val constr : string * string * repr list -> repr
end

functor Constructor
  (val module_name : string
   type exp
   type infixop
   type bind
   type pat
   type atype
   eqtype str
   type repr
   type reprrepr
   val construct_exp : ('a -> exp) -> ('a -> infixop) -> ('a -> bind) ->
                       ('a -> atype) -> ('a -> str) ->
                       (exp -> 'b) -> string * 'a list -> 'b
   val construct_infixop : ('a -> infixop) -> ('a -> exp) ->
                           (infixop -> 'b) ->
                            string * 'a list -> 'b
   val construct_bind : ('a -> exp) -> ('a -> pat) -> ('a -> str) -> (bind -> 'b) ->
                         string * 'a list -> 'b
   val construct_pat : ('a -> pat) -> ('a -> atype) ->
                       ('a -> str) -> (pat -> 'b) -> string * 'a list -> 'b
   val construct_atype : ('a -> atype) -> ('a -> str) ->
                         (atype -> 'b) -> string * 'a list -> 'b
   val construct_str : ('a -> str) ->
                       (str -> 'b) -> string * 'a list -> 'b
   val construct_reprrepr : (reprrepr -> 'a) -> string * repr list -> 'a
   structure Induction : Induction
       where type exp = exp
         and type infixop = infixop
         and type bind = bind
         and type pat = pat
         and type atype = atype
         and type str = str
         and type repr = repr
         and type reprrepr = reprrepr
  ) :> Constructor
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

   local
      open Induction
   in
      val constr = constructor
            construct_exp
            construct_infixop 
            construct_bind
            construct_pat
            construct_atype
            construct_str
            construct_reprrepr
   end
end
