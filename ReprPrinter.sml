signature ReprPrinter =
sig
    type repr
    val print : repr -> repr
end

functor ReprPrinter
   (type repr
    val printer : (repr -> repr -> repr) -> repr -> string * repr -> repr
    structure Induction : Induction
       where type repr = repr
    structure Constructor : Constructor
       where type repr = repr
    structure Deconstructor : Deconstructor
       where type repr = repr) :> ReprPrinter 
       where type repr = repr =
struct
   type repr = repr
   fun print repr =
      let val fromMLString = Induction.Repr.rep_str o Induction.Str.fromMLString
          val reprToString = Induction.reprToString
          val reprListToString = Induction.reprListToString
          fun out state s =
                 Constructor.constr ("output","out", [state, s])
          val init = Constructor.constr ("output","init",[])
          val print = printer out init
          val result = Induction.Repr.prt_repr
                       (fn e => print ("EXP",Induction.Repr.rep_exp e))
                       (fn i => print ("EXP",Induction.Repr.rep_exp
                                         (Induction.Exp.rep_infixopexp i)))
                       (fn b => print ("EXP",Induction.Repr.rep_exp
                                         (Induction.Exp.rep_bindexp b)))
                       (fn p => print ("PAT",Induction.Repr.rep_pat p))
                       (fn t => print ("ATYPE",Induction.Repr.rep_atype t))
                       (fn s => out init (Induction.Repr.rep_str s))
                       (fn rr =>
                            (case Deconstructor.deconstr (Induction.Repr.rep_reprrepr rr) of
                                ("exp",_,_) => print ("EXP",Induction.Repr.rep_reprrepr rr)
                              | ("infixopexp",_,_) => print ("EXP",Induction.Repr.rep_reprrepr rr)
                              | ("pat",_,_) => print ("PAT",Induction.Repr.rep_reprrepr rr)
                              | ("atype",_,_) => print ("ATYPE",Induction.Repr.rep_reprrepr rr)
                              | ("str",_,_) => out init (Induction.Repr.rep_reprrepr rr)
                              | ("reprrepr",_,_) => out init 
                                      (fromMLString (reprToString (Induction.Repr.rep_reprrepr rr)))
                              | (t,c,l) => raise Fail ("ReprPrinter.print(repr): no case "^t^"."^c^" "
                                                 ^(reprListToString l)))) repr
      in Constructor.constr ("output","result",[result])
      end
end
