signature Atype =
sig
   type atype
   eqtype str
   val rep_forall : str -> atype -> atype
   val rep_tyvar : str -> atype
   val rep_function : atype -> atype -> atype
   val prt_atype : (str -> 'a) -> (str -> atype -> 'a) -> (atype -> atype -> 'a) ->
                     atype -> 'a
   val deconstruct_atype : ('a -> 'b) -> ('c -> 'b) ->
                          (('c -> string * string * 'b list) ->
                           ('c -> 'a -> string * string * 'b list) ->
                           ('a -> 'a -> string * string * 'b list) -> 'd) -> 'd
   val construct_atype : ('a -> atype) -> ('a -> str) ->
                         (atype -> 'b) -> string * 'a list -> 'b
end

functor AbstractAtype
             (type atype
              eqtype str
              val rep_forall : str -> atype -> atype
              val rep_tyvar : str -> atype
              val rep_function : atype -> atype -> atype
              val prt_atype : (str -> 'a) ->
                              (str -> atype -> 'a) ->
                              (atype -> atype -> 'a) ->
                               atype -> 'a
            ) :> Atype
  where type atype = atype
    and type str = str =
struct
   type atype = atype
   type str = str
   val rep_forall = rep_forall
   val rep_tyvar = rep_tyvar
   val rep_function = rep_function
   val prt_atype = prt_atype
   fun deconstruct_atype atype str =
      fn f => f (fn v => ("atype","tyvar",[str v]))
                (fn v => fn t => ("atype","forall",[str v,atype t]))
                (fn t => fn t' => ("atype","function",[atype t,atype t']))
   fun construct_atype atype str rep_atype = 
      fn arg => rep_atype
           (case arg of ("tyvar",[v]) => rep_tyvar (str v)
                      | ("forall",[v,t]) => rep_forall (str v) (atype t)
                      | ("function",[t,t']) => rep_function (atype t) (atype t')
                      | (c,l) => raise Fail "atype: no case")
end
