signature Matcher =
sig
   type repr
   type reprrepr
   eqtype str
   structure ExpTypeSemantics : Semantics 
      where type repr = repr
   structure ExpTypeSubsSemantics : SubsSemantics 
      where type repr = repr
   structure ExpTypeSimpleConvention : SubsConvention
      where type repr = repr
        and type exstate = ExpTypeSubsSemantics.state
   structure FreeVariables : FreeVariables
      where type repr = repr
        and type str = str
   structure SimpleExpTypeSubstitute : Substitute
      where type repr = repr
   structure Occurs : OccursCheck
      where type repr = repr
   structure UnifySubsSemantics : Semantics
      where type repr = repr
   structure UnifySemantics : UnifySemantics
      where type repr = repr
        and type exstate = UnifySubsSemantics.state
   structure Unify : Unify
      where type repr = repr 
        and type exstate = UnifySemantics.state
end

functor Matcher
   (type repr
    type reprrepr
    eqtype str
    val incVar : str -> str
    structure Induction : Induction
       where type repr = repr
          and type reprrepr = reprrepr
          and type str = str
    structure Deconstructor : Deconstructor
       where type repr = repr
    structure Constructor : Constructor
       where type repr = repr
    datatype maparg = Copy of int | Update of int
    val map : ((string * string) * string * int list * maparg list) list
   ) :> Matcher
           where type repr = repr
             and type reprrepr = reprrepr
             and type str = str =
struct
   type repr = repr
   type reprrepr = reprrepr
   type str = str

   structure ExpTypeSemantics =
      MapJoinSemantics
        (val module_name = "ExpTypeSemantics"
         datatype maparg = datatype maparg
         type repr = repr
         val map_name = "match"
         val map = map
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
         structure Constructor : Constructor = Constructor
        ) :> Semantics where type repr = repr

   structure ExpTypeSubsSemantics =
      SubsSemantics
          (val module_name = "ExpTypeSubsSemantics"
           type repr = repr
           structure Semantics : Semantics = ExpTypeSemantics
           structure Induction : Induction = Induction
          ) :> SubsSemantics where type repr = repr

   structure ExpTypeSimpleConvention =
       SimpleSubs
          (val module_name = "ExpTypeSimpleConvention"
           type repr = repr
           type str = str
           val map_name = "match"
           val incVar = incVar
           structure Semantics : Semantics = ExpTypeSubsSemantics
           structure Induction : Induction = Induction
           structure Deconstructor : Deconstructor = Deconstructor
          ) :> SubsConvention where type repr = repr
                                and type exstate = ExpTypeSubsSemantics.state

   structure FreeVariables =
       FreeVariables
          (type repr = repr
           type str = str
           structure Induction : Induction = Induction
           structure Deconstructor : Deconstructor = Deconstructor) :> FreeVariables
                where type repr = repr
                  and type str = str

   structure SimpleExpTypeSubstitute =
      Substitute
        (val module_name = "SimpleExpTypeSubstitute"
         type repr = repr
         type state = unit
         val map_name = "substitute"
         val initstate = ()
         type str = str
         val incVar = incVar
         structure SubsSemantics : SubsSemantics = ExpTypeSubsSemantics
         structure SubsConvention : SubsConvention = ExpTypeSimpleConvention
         structure FreeVariables : FreeVariables = FreeVariables
         structure Semantics : Semantics = ExpTypeSemantics
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
        ) :> Substitute where type repr = repr

   structure Occurs = OccursCheck(type repr = repr
         val map_name = "match"
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
        ) :> OccursCheck where type repr = repr

   structure UnifySubsSemantics =
      MapJoinSemantics
        (val module_name = "UnifySubsSemantics"
         datatype maparg = datatype maparg
         type repr = repr
         val map_name = "match"
         val map = map
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
         structure Constructor : Constructor = Constructor
        ) :> Semantics where type repr = repr

   structure UnifySemantics = UnifySemantics
      (val module_name = "UnifySemantics"
       type repr = repr
       type state = ExpTypeSemantics.state
       structure Substitute : Substitute = SimpleExpTypeSubstitute
       structure Semantics : Semantics = UnifySubsSemantics
       structure Deconstructor : Deconstructor = Deconstructor
       structure Induction : Induction = Induction
      ) :> UnifySemantics
            where type repr = repr
              and type exstate = UnifySubsSemantics.state

    structure Unify = Unify
       (val module_name = "Unify"
        type repr = repr
        val map_name = "match"
        structure Induction : Induction = Induction
        structure Constructor : Constructor = Constructor
        structure Deconstructor : Deconstructor = Deconstructor
        structure Occurs : OccursCheck = Occurs
        structure Semantics : Semantics = UnifySemantics
       ) :> Unify
            where type repr = repr 
              and type exstate = UnifySemantics.state
end
