signature Pat =
sig
   type pat
   type atype
   eqtype str
   val rep_bindvar : str -> atype -> pat
   val rep_pair : pat -> pat -> pat
   val rep_alias : str -> pat -> pat
   val prt_pat : (unit -> 'a) -> (atype -> 'a) -> (str -> atype -> 'a) ->
                 (pat -> pat -> 'a) -> (str -> pat -> 'a) -> pat -> 'a
   val deconstruct_pat : ('a -> 'b) -> ('c -> 'b) -> ('d -> 'b) ->
         ((unit -> string * string * 'e list) -> ('c -> string * string * 'b list) ->
          ('d -> 'c -> string * string * 'b list) ->
          ('a -> 'a -> string * string * 'b list) ->
          ('d -> 'a -> string * string * 'b list) -> 'f) -> 'f
   val construct_pat : ('a -> pat) -> ('a -> atype) ->
                       ('a -> str) -> (pat -> 'b) -> string * 'a list -> 'b
end

functor AbstractPat
           (type pat
            type atype
            eqtype str
            val rep_bindvar : str -> atype -> pat
            val rep_pair : pat -> pat -> pat
            val rep_nil : unit -> pat
            val rep_wildcard : atype -> pat
            val rep_alias : str -> pat -> pat
            val prt_pat : (unit -> 'a) -> (atype -> 'a) -> (str -> atype -> 'a) ->
                          (pat -> pat -> 'a) -> (str -> pat -> 'a) -> pat -> 'a
           ) :> Pat
  where type pat = pat
    and type atype = atype
    and type str = str =
struct
   type pat = pat
   type atype = atype
   type str = str
   val rep_bindvar = rep_bindvar
   val rep_pair = rep_pair
   val rep_nil = rep_nil
   val rep_wildcard = rep_wildcard
   val rep_alias = rep_alias
   val prt_pat = prt_pat
   fun deconstruct_pat pat atype str =
      fn f => f (fn () => ("pat","nil",[]))
                (fn t => ("pat","wildcard",[atype t]))
                (fn v => fn t => ("pat","bindvar",[str v,atype t]))
                (fn p => fn p' => ("pat","pair", [pat p, pat p']))
                (fn v => fn p => ("pat","alias",[str v, pat p]))
   fun construct_pat pat atype str rep_pat = 
      fn arg => rep_pat
           (case arg of ("nil",[]) => rep_nil ()
                      | ("wildcard",[t]) => rep_wildcard (atype t)
                      | ("bindvar",[v,t]) => rep_bindvar (str v) (atype t)
                      | ("pair",[p,p']) => rep_pair (pat p) (pat p')
                      | ("alias",[v,p]) => rep_alias (str v) (pat p)
                      | (c,l) => raise Fail "pat: no case")
end
