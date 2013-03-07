signature SystemF =
sig
   val module_name : string
   type str = string

   datatype atype = Forall of str * atype
                  | Tyvar of str
                  | Function of atype * atype

   datatype pat = Pair of pat * pat
                | Alias of str * pat
                | BindVar of str * atype
                | Wildcard of atype
                | Nil

   datatype exp = Comb of exp * exp
                | PairExp of exp * exp
                | Var of str
                | BindExp of bind
                | InfixopExp of infixop
                | AtypeExp of atype
    and infixop = Infixop of exp
                | InfixComb of exp * infixop
       and bind = Abs of pat * exp
                | Let of pat * exp * exp
                | Lambda of str * exp

   datatype repr = Exp of exp
                 | InfixopRep of infixop
                 | Bind of bind
                 | Pat of pat
                 | Atype of atype
                 | Str of str
                 | ReprRepr of reprrepr
   and reprrepr = ReprNil | Repr of repr * repr

   structure Exp : Exp
     where type exp = exp
       and type bind = bind
       and type atype = atype
       and type str = str

   structure Infixop : Infixop
     where type infixop = infixop
       and type exp = exp

   structure Bind : Bind
     where type bind = bind
       and type exp = exp
       and type pat = pat
       and type atype = atype
       and type str = str

   structure Pat : Pat
     where type pat = pat
       and type atype = atype
       and type str = str

   structure Atype : Atype
     where type atype = atype
       and type str = str

   structure Str : Str
       where type str = string

   structure Repr : Repr
       where type repr = repr

   structure ReprRepr : ReprRepr
       where type repr = repr
         and type reprrepr = reprrepr

   structure Induction : Induction
     where type exp = exp
       and type bind = bind
       and type pat = pat
       and type atype = atype
       and type str = str

   structure Deconstructor : Deconstructor
     where type repr = Induction.repr

   structure Constructor : Constructor
     where type repr = Induction.repr

end
