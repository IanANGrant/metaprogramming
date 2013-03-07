signature Repr =
sig
   type repr
   type exp
   type infixop
   type bind
   type pat
   type atype
   type str
   type reprrepr
   val rep_exp : exp -> repr
   val rep_infixop : infixop -> repr
   val rep_bind : bind -> repr
   val rep_pat : pat -> repr
   val rep_atype : atype -> repr
   val rep_str : str -> repr
   val rep_reprrepr : reprrepr -> repr
   val prt_repr : (exp -> 'a) ->
                  (infixop -> 'a) ->
                  (bind -> 'a) ->
                  (pat -> 'a) -> 
                  (atype -> 'a) -> 
                  (str -> 'a) -> 
                  (reprrepr -> 'a) ->
                   repr -> 'a
   val deconstruct_repr :
         ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> ('i -> 'j) ->
         ('k -> 'l) -> ('m -> 'n) ->
        (('a -> string * string * 'b list) -> ('c -> string * string * 'd list) ->
         ('e -> string * string * 'f list) -> ('g -> string * string * 'h list) ->
         ('i -> string * string * 'j list) -> ('k -> string * string * 'l list) ->
         ('m -> string * string * 'n list) -> 'o) -> 'o
   val construct_repr :
        ('a -> exp) -> ('a -> infixop) ->
        ('a -> bind) -> ('a -> pat) -> ('a -> atype) -> ('a -> str) ->
        ('a -> reprrepr) -> (repr -> 'b) -> string * 'a list -> 'b
end

functor AbstractRepr
  (type repr
   type exp
   type infixop
   type bind
   type pat
   type atype
   eqtype str
   type reprrepr
   val rep_exp : exp -> repr
   val rep_infixop : infixop -> repr
   val rep_bind : bind -> repr
   val rep_pat : pat -> repr
   val rep_atype : atype -> repr
   val rep_str : str -> repr
   val rep_reprrepr : reprrepr -> repr
   val prt_repr : (exp -> 'a) ->
                  (infixop -> 'a) ->
                  (bind -> 'a) ->
                  (pat -> 'a) -> 
                  (atype -> 'a) -> 
                  (str -> 'a) -> 
                  (reprrepr -> 'a) ->
                   repr -> 'a
  ) :> Repr
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
   val rep_exp = rep_exp
   val rep_infixop = rep_infixop
   val rep_bind = rep_bind
   val rep_pat = rep_pat
   val rep_atype = rep_atype
   val rep_str = rep_str
   val rep_reprrepr = rep_reprrepr
   val prt_repr = prt_repr
   fun deconstruct_repr exp infixop bind pat atype str reprrepr =
      fn f => f (fn e => ("repr","exp", [exp e]))
                (fn i => ("repr","infixop", [infixop i]))
                (fn b => ("repr","bind", [bind b]))
                (fn p => ("repr","pat", [pat p]))
                (fn t => ("repr","atype", [atype t]))
                (fn s => ("repr","str", [str s]))
                (fn r => ("repr","reprrepr",[reprrepr r]))
   fun construct_repr exp infixop bind pat atype str reprrepr rep_repr =
      fn arg => rep_repr
           (case arg of ("exp",[e]) => rep_exp (exp e)
                      | ("infixop",[i]) => rep_infixop (infixop i)
                      | ("bind",[b]) => rep_bind (bind b)
                      | ("pat",[p]) => rep_pat (pat p)
                      | ("atype",[t]) => rep_atype (atype t)
                      | ("str",[s]) => rep_str (str s)
                      | ("reprrepr",[r]) => rep_reprrepr (reprrepr r)
                      | (c,l) => raise Fail "repr: no case")
end
