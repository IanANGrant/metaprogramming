structure SystemF :> SystemF =
struct
   val module_name = "SystemF"
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

   structure Exp = 
      AbstractExp
         (val module_name = module_name^".Exp"
          type exp = exp
          type infixop = infixop
          type bind = bind
          type atype = atype
          type str = str
          val rep_bindexp = fn b => BindExp b
          val rep_infixopexp = fn i => InfixopExp i
          val rep_var = fn v => Var v
          val rep_atypeexp = fn t => AtypeExp t
          val rep_comb = fn e => fn e' => Comb (e,e')
          val rep_pairexp = fn e => fn e' => PairExp (e,e')
          val prt_exp = fn infixopexp => fn bindexp => fn atypeexp => fn var =>
                 fn comb => fn pairexp => 
             fn BindExp b => bindexp b
              | InfixopExp i => infixopexp i
              | Var v => var v
              | AtypeExp t => atypeexp t
              | Comb (e,e') => comb e e'
              | PairExp (e,e') => pairexp e e'
         ) :> Exp
     where type exp = exp
       and type atype = atype
       and type infixop = infixop
       and type bind = bind
       and type str = str

   structure Infixop = 
       AbstractInfixop
          (val module_name = module_name^".Infixop"
           type infixop = infixop
           type exp = exp
           val rep_exp = fn e => Infixop e
           val rep_comb = fn e => fn es => InfixComb (e,es)
           val prt_infixop = fn exp => fn comb =>
              fn Infixop e => exp e
               | InfixComb (e,es) => comb e es
          ) :> Infixop
     where type infixop = infixop
       and type exp = exp

   structure Bind = 
       AbstractBind
          (val module_name = module_name^".Bind"
           type bind = bind
           type exp = exp
           type atype = atype
           type pat = pat
           type str = str
           val rep_abs = fn p => fn e => Abs (p,e)
           val rep_let = fn p => fn e => fn e' => Let (p,e,e')
           val rep_lambda = fn v => fn e => Lambda (v,e)
           val prt_bind = fn abs => fn lambda => fn let' => 
              fn Abs (p,e) => abs p e
               | Let (p,e,e') => let' p e e'
               | Lambda (v,e) => lambda v e
          ) :> Bind
     where type bind = bind
       and type exp = exp
       and type atype = atype
       and type pat = pat
       and type str = str

   structure Pat = 
      AbstractPat
         (val module_name = module_name^".Pat"
          type pat = pat
          type atype = atype
          type str = str
          val rep_bindvar = fn v => fn t => BindVar (v,t)
          val rep_pair = fn p => fn p' => Pair (p,p')
          val rep_nil = fn () => Nil
          val rep_wildcard = fn t => Wildcard t
          val rep_alias = fn v => fn p => Alias (v,p)
          val prt_pat = fn nil' => fn wildcard => fn bindvar => fn pair => fn alias =>  
             fn BindVar (v,t) => bindvar v t
              | Pair (p,p') => pair p p'
              | Alias (v,p) => alias v p
              | Wildcard t => wildcard t
              | Nil => nil' ()
         ) :> Pat
     where type pat = pat
       and type atype = atype
       and type str = str

   structure Atype = 
      AbstractAtype
         (val module_name = module_name^".Atype"
          type atype = atype 
          type str = str
          val rep_forall = fn v => fn t => Forall (v,t)
          val rep_tyvar = fn v => Tyvar v
          val rep_function = fn t => fn t' => Function (t,t')
          val prt_atype = fn tyvar => fn forall => fn function =>
              fn Forall (v,t) => forall v t
               | Tyvar v => tyvar v
               | Function (t,t') => function t t'
         ) :> Atype
     where type atype = atype
       and type str = str

   structure Str = 
      AbstractStr
         (val module_name = module_name^".Str"
          type str = str
          val rep_string = fn s:str => s
          val prt_string = fn str => 
                fn s:str => str s
          val toMLString = fn s => s 
          val fromMLString = fn s => s) :> Str
     where type str = str

   structure Repr =
     AbstractRepr
      (val module_name = module_name^".Repr"
       type repr = repr
       type exp = exp       
       type infixop = infixop
       type bind = bind
       type pat = pat
       type atype = atype
       type str = str
       type reprrepr = reprrepr
       val rep_exp = fn e => Exp e
       val rep_infixop = fn i => InfixopRep i
       val rep_bind = fn b => Bind b
       val rep_pat = fn p => Pat p
       val rep_atype = fn t => Atype t
       val rep_str = fn s => Str s
       val rep_reprrepr = fn r => ReprRepr r
       val prt_repr = fn exp => fn infixop => fn bind => fn pat => 
                      fn atype => fn str => fn reprrepr => 
             fn Exp e => exp e
              | InfixopRep i => infixop i
              | Bind b => bind b
              | Pat p => pat p
              | Atype t => atype t
              | Str s => str s
              | ReprRepr r => reprrepr r
  ) :> Repr
    where type repr = repr
      and type exp = exp
      and type infixop = infixop
      and type bind = bind
      and type pat = pat
      and type atype = atype
      and type str = str
      and type reprrepr = reprrepr

   structure ReprRepr =
     AbstractReprRepr
      (val module_name = module_name^".ReprRepr"
       type reprrepr = reprrepr
       type repr = repr
       val rep_repr = fn r => fn r' => Repr (r,r')
       val rep_nil = fn () => ReprNil
       val prt_reprrepr =
          fn reprnil => fn repr =>
             fn Repr (a,b) => repr a b
              | ReprNil => reprnil ()
  ) :> ReprRepr
    where type repr = repr
      and type reprrepr = reprrepr

   structure Induction =
     Induction
      (val module_name = module_name^".Induction"
       type exp = exp
       type atype = atype
       type infixop = infixop
       type bind = bind
       type pat = pat
       type str = str
       type repr = repr
       type reprrepr = reprrepr
       structure Exp : Exp where type exp = exp
                             and type bind = bind
                             and type atype = atype
                             and type str = str = Exp
       structure Infixop : Infixop where type infixop = infixop
                                     and type exp = exp = Infixop
       structure Bind : Bind where type bind = bind
                               and type exp = exp
                               and type atype = atype
                               and type pat = pat = Bind
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
      ) :> Induction
           where type exp = exp
             and type infixop = infixop
             and type bind = bind
             and type pat = pat
             and type atype = atype
             and type str = str
             and type repr = repr
             and type reprrepr = reprrepr

   structure Deconstructor =
      Deconstructor
           (val module_name = module_name^".Deconstructor"
            type repr = repr
            type exp = exp
            type infixop = infixop
            type bind = bind
            type pat = pat
            type atype = atype
            type str = str
            type reprrepr = reprrepr
            val prt_exp = Exp.prt_exp
            val deconstruct_exp = Exp.deconstruct_exp
            val prt_infixop = Infixop.prt_infixop
            val deconstruct_infixop = Infixop.deconstruct_infixop
            val prt_bind = Bind.prt_bind
            val deconstruct_bind = Bind.deconstruct_bind
            val prt_pat = Pat.prt_pat
            val deconstruct_pat = Pat.deconstruct_pat
            val prt_atype = Atype.prt_atype
            val deconstruct_atype = Atype.deconstruct_atype
            val prt_string = Str.prt_string
            val deconstruct_str = Str.deconstruct_str
            val prt_reprrepr = ReprRepr.prt_reprrepr
            val deconstruct_reprrepr = ReprRepr.deconstruct_reprrepr
            structure Induction : Induction = Induction) :> Deconstructor
               where type repr = repr

   structure Constructor =
       Constructor
           (val module_name = module_name^".Constructor"
            type repr = repr
            type exp = exp
            type infixop = infixop
            type bind = bind
            type pat = pat
            type atype = atype
            type str = str
            type reprrepr = reprrepr
            val construct_exp = Exp.construct_exp
            val construct_infixop = Infixop.construct_infixop
            val construct_bind = Bind.construct_bind
            val construct_pat = Pat.construct_pat
            val construct_atype = Atype.construct_atype
            val construct_str = Str.construct_str
            val construct_reprrepr = ReprRepr.construct_reprrepr
            structure Deconstructor : Deconstructor = Deconstructor
            structure Induction : Induction = Induction) :> Constructor
               where type repr = repr

end
