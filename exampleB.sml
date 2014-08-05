load "Syntax";
load "SystemF";
load "MapJoinSemantics";
load "Substitute";
load "SimpleSubs";
load "ReprEmbedding2";
load "UnifySemantics";
load "Unification";
load "Unit";

load "Unifier";
load "Matcher";

local
   open SystemF
   open SystemF.Induction

   structure Syntax = Syntax
      (type repr = repr
       type str = str
       val grammar = "grammar.txt"
       structure Induction : Induction = Induction
       structure Deconstructor : Deconstructor = Deconstructor
       structure Constructor : Constructor = Constructor) 

   structure ReprEmbedding = ReprEmbedding2
     (val module_name = "ReprEmbedding"
      type str = str
      type repr = repr
      type reprrepr = reprrepr
      structure Induction : Induction = Induction
      structure Deconstructor : Deconstructor = Deconstructor
      structure Constructor : Constructor = Constructor
     ) :> ReprEmbedding
          where type str = str
            and type repr = repr
            and type reprrepr = reprrepr 

   structure UnitM = Unit
     (val module_name = "UnitM"
      type repr = repr
      structure Deconstructor : Deconstructor = Deconstructor
      structure Constructor : Constructor = ReprEmbedding.Constructor
      structure Induction : Induction = Induction) :> Unit
            where type repr = repr

   structure EmbeddedSyntax = Syntax
      (type repr = repr
       type str = str
       val grammar = "grammar2.txt"
       structure Induction : Induction = Induction
       structure Deconstructor : Deconstructor = ReprEmbedding.Deconstructor
       structure Constructor : Constructor = ReprEmbedding.Constructor) 

   datatype maparg = Copy of int | Update of int

                   (* α                 μ.β                        MAP     JOIN *)
   val exp_map = [(("exp","var"),     "varref",     [1],    [Update 1]),
                  (("pat","bindvar"), "patvar",     [1],    [Update 1,Copy 2]),
                  (("pat","alias"),   "patalias",   [1,2],  [Update 1,Update 2]),
                  (("bind","abs"),    "patabsbind", [1,2],  [Update 1,Update 2]),
                  (("bind","let"),    "patletbind", [1,2,3],[Update 1,Update 2,Update 3])]

   (* exp_umap is for unification. It is the same as exp_map except
      that we substitute in the types of variable bindings. *)

   val exp_umap = [(("exp","var"),     "varref",     [1],    [Update 1]),
                   (("pat","bindvar"), "patvar",     [1,2],  [Update 1,Update 2]),
                   (("pat","alias"),   "patalias",   [1,2],  [Update 1,Update 2]),
                   (("bind","abs"),    "patabsbind", [1,2],  [Update 1,Update 2]),
                   (("bind","let"),    "patletbind", [1,2,3],[Update 1,Update 2,Update 3])]

   val match_map = [(("exp","meta"),    "varref",     [1],    [Update 1]),
                    (("atype","meta"),  "varref",     [1],    [Update 1]),
                    (("atyvar","meta"), "varref",     [1],    [Update 1]),
                    (("bindvar","meta"),"varref",     [1],    [Update 1])(*,
                    (("exp","var"),     "exp.var",             [1],    [Update 1]),
                    (("atype","tyvar"), "atype.tyvar",         [1],    [Update 1]),
                    (("atyvar","var"),  "atyvar.var",          [1],    [Update 1]),
                    (("bindvar","var"), "bindvar.var",         [1],    [Update 1])*)]

   val type_map = [(("bind","lambda"), "absbind",   [1,2],  [Update 1,Update 2]),
                   (("atype","forall"),"absbind",   [1,2],  [Update 1,Update 2]),
                   (("atype","tyvar"), "varref",    [1],    [Update 1])]

   structure Unifier = 
        Unifier(type repr = repr
                type reprrepr = reprrepr
                type str = str
                fun incVar v = v^"'"
                structure Induction : Induction = Induction
                structure Deconstructor : Deconstructor = Deconstructor
                structure Constructor : Constructor = Constructor
                datatype maparg = datatype maparg
                val map = exp_umap@type_map)

   structure Matcher = 
        Matcher(type repr = repr
                type reprrepr = reprrepr
                type str = str
                fun incVar v = v^"'"
                structure Induction : Induction = Induction
                structure Deconstructor : Deconstructor = ReprEmbedding.Deconstructor
                structure Constructor : Constructor = ReprEmbedding.Constructor
                datatype maparg = datatype maparg
                val map = match_map)

   structure ExpSemantics =
      MapJoinSemantics
        (val module_name = "ExpSemantics"
         datatype maparg = datatype maparg
         type repr = repr
         val map_name = "barendregt"
         val map = exp_map
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
         structure Constructor : Constructor = Constructor
        ) :> Semantics where type repr = repr

   structure TypeSemantics =
      MapJoinSemantics
        (val module_name = "TypeSemantics"
         datatype maparg = datatype maparg
         type repr = repr
         val map_name = "barendregt"
         val map = type_map
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
         structure Constructor : Constructor = Constructor
        ) :> Semantics where type repr = repr

   structure ExpSubsSemantics =
      SubsSemantics
          (val module_name = "ExpSubsSemantics"
           type repr = repr
           structure Semantics : Semantics = ExpSemantics
           structure Induction : Induction = Induction
          ) :> SubsSemantics where type repr = repr

   structure TypeSubsSemantics =
      SubsSemantics
          (val module_name = "TypeSubsSemantics"
           type repr = repr
           structure Semantics : Semantics = TypeSemantics
           structure Induction : Induction = Induction
          ) :> SubsSemantics where type repr = repr

   structure ExpBarendregtConvention =
       Barendregt
          (val module_name = "ExpBarendregtConvention"
           type repr = repr
           type str = str
           val map_name = "barendregt"
           fun incVar v = v^"'"
           structure Semantics : Semantics = ExpSubsSemantics
           structure Induction : Induction = Induction
           structure Deconstructor : Deconstructor = Deconstructor
          ) :> SubsConvention where type repr = repr
                                and type exstate = ExpSubsSemantics.state

   structure TypeBarendregtConvention =
       Barendregt
          (val module_name = "TypeBarendregtConvention"
           type repr = repr
           type str = str
           val map_name = "barendregt"
           fun incVar v = v^"'"
           structure Semantics : Semantics = TypeSubsSemantics
           structure Induction : Induction = Induction
           structure Deconstructor : Deconstructor = Deconstructor
          ) :> SubsConvention where type repr = repr
                                and type exstate = TypeSubsSemantics.state

   structure ExpSimpleConvention =
       SimpleSubs
          (val module_name = "ExpSimpleConvention"
           type repr = repr
           type str = str
           val map_name = "barendregt"
           fun incVar v = v^"'"
           structure Semantics : Semantics = ExpSubsSemantics
           structure Induction : Induction = Induction
           structure Deconstructor : Deconstructor = Deconstructor
          ) :> SubsConvention where type repr = repr
                                and type exstate = ExpSubsSemantics.state

   structure TypeSimpleConvention =
       SimpleSubs
          (val module_name = "TypeSimpleConvention"
           type repr = repr
           type str = str
           val map_name = "barendregt"
           fun incVar v = v^"'"
           structure Semantics : Semantics = TypeSubsSemantics
           structure Induction : Induction = Induction
           structure Deconstructor : Deconstructor = Deconstructor
          ) :> SubsConvention where type repr = repr
                                and type exstate = TypeSubsSemantics.state

   structure FreeVariables =
       FreeVariables
          (type repr = repr
           type str = str
           structure Induction : Induction = Induction
           structure Deconstructor : Deconstructor = Deconstructor) :> FreeVariables
                where type repr = repr
                  and type str = str

   structure ExpSubstitute =
      Substitute
        (val module_name = "ExpSubstitute"
         type repr = repr
         type state = unit
         val map_name = "substitute"
         val initstate = ()
         type str = str
         fun incVar v = v^"'"
         structure SubsSemantics : SubsSemantics = ExpSubsSemantics
         structure SubsConvention : SubsConvention = ExpBarendregtConvention
         structure FreeVariables : FreeVariables = FreeVariables
         structure Semantics : Semantics = ExpSemantics
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
        ) :> Substitute where type repr = repr

   structure TypeSubstitute =
      Substitute
        (val module_name = "TypeSubstitute"
         type repr = repr
         type state = unit
         val map_name = "substitute"
         val initstate = ()
         type str = str
         fun incVar v = v^"'"
         structure SubsSemantics : SubsSemantics = TypeSubsSemantics
         structure SubsConvention : SubsConvention = TypeBarendregtConvention
         structure FreeVariables : FreeVariables = FreeVariables
         structure Semantics : Semantics = TypeSemantics
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
        ) :> Substitute where type repr = repr

   structure SimpleExpSubstitute =
      Substitute
        (val module_name = "SimpleExpSubstitute"
         type repr = repr
         type state = unit
         val map_name = "substitute"
         val initstate = ()
         type str = str
         fun incVar v = v^"'"
         structure SubsSemantics : SubsSemantics = ExpSubsSemantics
         structure SubsConvention : SubsConvention = ExpSimpleConvention
         structure FreeVariables : FreeVariables = FreeVariables
         structure Semantics : Semantics = ExpSemantics
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
        ) :> Substitute where type repr = repr

   structure SimpleTypeSubstitute =
      Substitute
        (val module_name = "SimpleTypeSubstitute"
         type repr = repr
         type state = unit
         val map_name = "substitute"
         val initstate = ()
         type str = str
         fun incVar v = v^"'"
         structure SubsSemantics : SubsSemantics = TypeSubsSemantics
         structure SubsConvention : SubsConvention = TypeSimpleConvention
         structure FreeVariables : FreeVariables = FreeVariables
         structure Semantics : Semantics = TypeSemantics
         structure Induction : Induction = Induction
         structure Deconstructor : Deconstructor = Deconstructor
        ) :> Substitute where type repr = repr

(*
   val tcexp_map = [(("exp","var"),       ("varref"),     [1]),
                    (("exp","comb"),      ("comb"),       [1,2]),
                    (("exp","pairexp"),   ("pairexp"),    [1,2]),
                    (("exp","atypeexp"),  ("id"),         [1]),
                    (("exp","bindexp"),   ("id"),         [1]),
                    (("pat","bindvar"),   ("patvar"),     [1,2]),
                    (("pat","alias"),     ("patalias"),   [1,2]),
                    (("pat","pair"),      ("patpair"),    [1,2]),
                    (("pat","wildcard"),  ("wildcard"),   [1]),
                    (("bind","lambda"),   ("lambda"),     [1,2]),
                    (("bind","abs"),      ("patabsbind"), [1,2]),
                    (("bind","let"),      ("patletbind"), [1,2,3])]

   val tctype_map = [(("atype","forall"),   ("forall"),   [1,2]),
                     (("atype","tyvar"),    ("id"),       [1]),
                     (("atype","function"), ("function"), [1,2])]
*)
in
   exception Unify = Unifier.Unify.Unify
   val t =  ReprEmbedding.Constructor.constr ("exp","var",[Induction.Repr.rep_str "x"])
   val t' = Induction.reprToString t
   val (t'',c,l) =  ReprEmbedding.Deconstructor.deconstr t
   val t''' = Induction.reprListToString l

   val represent = UnitM.unit

   fun unify x y = Unifier.Unify.reconstr Unifier.Unify.initstate 
                         (ReprEmbedding.Constructor.constr ("unify","unify",[x,y]))
   val unify_mgu = Unifier.UnifySemantics.get_mgu o Unifier.Unify.get_state

   fun match x y = Matcher.Unify.reconstr Matcher.Unify.initstate 
                         (ReprEmbedding.Constructor.constr ("match","unify",[x,y]))
   val match_mgu = Matcher.UnifySemantics.get_mgu o Matcher.Unify.get_state

   val toString = Induction.dec_repr_str Induction.Repr.prt_repr
   val UTF8 = toString o Syntax.UTF8Printer.print
   val TeX = toString o Syntax.TeXPrinter.print
   val sem = Induction.reprToString

   val emb_sem = ReprEmbedding.reprToString

   val emb_parser_ = EmbeddedSyntax.repr_parser_

   val emb_repr_parser = EmbeddedSyntax.repr_parser
   val emb_UTF8 = toString o EmbeddedSyntax.UTF8Printer.print
   val emb_TeX = toString o EmbeddedSyntax.TeXPrinter.print

   fun exp_parser s = Induction.dec_repr_exp
                      Induction.Repr.prt_repr
                      (Syntax.repr_parser s)

   fun atype_parser s = 
         case exp_parser s of
             (* InfixopExp (Infixop( *) AtypeExp t (* ))*) => t 
           | _ => raise Fail ("atype_parser: not a type "^s)

   val repr_parser = Syntax.repr_parser

   val rep_exp_parser = Induction.Repr.rep_exp o exp_parser
   val rep_atype_parser = Induction.Repr.rep_atype o atype_parser

   val rep_str = Induction.Repr.rep_str o Induction.Str.rep_string

   fun substitute reconstr initstate v e e' =
      let val (_,state) = 
             reconstr initstate ("substitute","addsubs",[rep_str v,e'])
          val (result,_) = 
             reconstr state ("substitute","subs",[e])
      in result
      end

   fun substitute_list reconstr initstate parallel l e =
      let fun iter state [] = state
            | iter state ((v,e)::ss) =
               let val (_,state) = reconstr state ("substitute",parallel,[rep_str v,e])
               in iter state ss end
          val (result,_) = reconstr (iter initstate l) ("substitute","subs",[e])
      in result
      end

   fun setsubs reconstr parser initstate l =
      let fun iter state [] = state
            | iter state ((e,v)::ss) =
                let val (_,state) = reconstr state ("substitute","parallel",[rep_str v,e])
                in iter state ss
                end
          val l' = List.map (fn (e,v) => (parser e,v)) l
      in reconstr (iter initstate l') ("substitute","getsubs",[])
      end

   fun getsubs reconstr state =
       let val (res,_) = reconstr state ("substitute","getsubs",[])
       in res end

   fun compsubs reconstr state ss =
      let val (_,state') = reconstr state ("substitute","compose",[ss])
      in state'
      end

   val expsubs = substitute ExpSubstitute.reconstr ExpSubstitute.initstate
   val typesubs = substitute TypeSubstitute.reconstr TypeSubstitute.initstate
   val simple_expsubs = substitute Unifier.SimpleExpTypeSubstitute.reconstr 
                                   Unifier.SimpleExpTypeSubstitute.initstate
   val simple_typesubs = substitute Unifier.SimpleExpTypeSubstitute.reconstr
                                    Unifier.SimpleExpTypeSubstitute.initstate

   val expsubs_list = substitute_list ExpSubstitute.reconstr ExpSubstitute.initstate
   val typesubs_list = substitute_list TypeSubstitute.reconstr TypeSubstitute.initstate
   val simple_expsubs_list = substitute_list Unifier.SimpleExpTypeSubstitute.reconstr
                                             Unifier.SimpleExpTypeSubstitute.initstate
   val simple_typesubs_list = substitute_list Unifier.SimpleExpTypeSubstitute.reconstr
                                              Unifier.SimpleExpTypeSubstitute.initstate

   val setexpsubs = setsubs ExpSubstitute.reconstr rep_exp_parser
                            ExpSubstitute.initstate
   val getexpsubs = getsubs ExpSubstitute.reconstr
   val compexpsubs = compsubs ExpSubstitute.reconstr 
   val idexpsubs = ExpSubstitute.initstate

   val simple_setexpsubs = setsubs Unifier.SimpleExpTypeSubstitute.reconstr rep_exp_parser
                                   Unifier.SimpleExpTypeSubstitute.initstate
   val simple_getexpsubs = getsubs Unifier.SimpleExpTypeSubstitute.reconstr
   val simple_compexpsubs = compsubs Unifier.SimpleExpTypeSubstitute.reconstr
   val simple_idexpsubs = Unifier.SimpleExpTypeSubstitute.initstate

   val simple_settypesubs = setsubs Unifier.SimpleExpTypeSubstitute.reconstr rep_atype_parser
                                    Unifier.SimpleExpTypeSubstitute.initstate
   val simple_gettypesubs = getsubs Unifier.SimpleExpTypeSubstitute.reconstr
   val simple_comptypesubs = compsubs Unifier.SimpleExpTypeSubstitute.reconstr
   val simple_idtypesubs = Unifier.SimpleExpTypeSubstitute.initstate

   local open Induction Induction.Repr Induction.ReprRepr
   in
      val r_rr = dec_repr_repr prt_repr
      val prt_reprrepr = prt_reprrepr
      val reprnil = rep_reprrepr (rep_nil ())
      fun reprcons a l = rep_reprrepr (rep_repr a l)
   end

   fun foldright f =
       let val rec iter = fn r => prt_reprrepr
              (fn () => r)
              (fn a => fn b => iter (f a r) (r_rr b))
       in iter
       end

   fun foldleft f =
       let val rec iter = fn r => prt_reprrepr
               (fn () => r)
                 (fn a => fn b => f a (iter r (r_rr b)))
       in iter
       end

   fun isnil r = prt_reprrepr (fn () => true) (fn _ => fn _ => false) (r_rr r)

   val reverse = foldright reprcons reprnil

   fun subsrtolist ss =
      rev (foldright (fn a => fn r =>
                         (prt_reprrepr
                            (fn () => raise (Fail "nil pair"))
                            (fn v => fn e => (toString v,e)) (r_rr a))::r)
                [] (r_rr ss))

   fun subsltostring fmt l =
      List.foldl (fn ((v,e),r) => r^(if r = "" then "" else ",")^(fmt e)^"/"^v) "" l

   fun subsrtostring fmt ss =
      subsltostring fmt (subsrtolist ss)

   fun substest fmt ty parallel es l =
      let val (substitute,parser) =
            case ty of
                 "exp" => (expsubs_list parallel,rep_exp_parser)
               | "type" => (typesubs_list parallel,rep_atype_parser)
               | "simple_exp" => (simple_expsubs_list parallel,rep_exp_parser)
               | "simple_type" => (simple_typesubs_list parallel,rep_atype_parser)
               | _ => raise Fail ("No type "^ty)
          val e = rep_exp_parser es
          val l' = List.map (fn (e,v) => (v, parser e)) l
          val r = substitute l' e
          val rs = fmt r
          val es = fmt e
      in es^"["^(subsltostring fmt l')^"] = "^rs
      end

   fun repreq e e' = 
       let val (_,s) = unify e e'
       in isnil (unify_mgu s)
       end

   fun dst_forall t =
       let val (_,s) = unify (Induction.Repr.rep_exp (Induction.Exp.rep_atypeexp (
                                 (Induction.Atype.rep_forall (Induction.Str.rep_string "X")
                                 (Induction.Atype.rep_tyvar (Induction.Str.rep_string "Y")))))) t
       in subsrtostring UTF8 (unify_mgu s)
       end
end

val (subs1,state) = simple_setexpsubs [("b","a"),("c","d")]
val subs1' = subsrtostring UTF8 subs1
val (subs2,_) = simple_setexpsubs [("e","b"),("f","c")]
val subs2' = subsrtostring UTF8 subs2
val state = simple_compexpsubs state subs2
val subs3 = simple_getexpsubs state
val subs3' = subsrtostring UTF8 subs3

val reprt = emb_repr_parser ("ΛY.λx:⟪ΠX.X⟫.⟪ΛX.Y ⟪Ω⟫⟫")
val reprt' = (emb_UTF8  reprt, emb_sem reprt)
val metat = emb_repr_parser ("λ⟦λ⌜v⌝:⌜T⌝.⌜e⌝ ⌜v⌝⟧:X.⟦λ⌜v⌝:⌜T⌝.⌜e⌝ ⌜v⌝⟧")
val metat' = (emb_UTF8  metat, emb_sem metat)

val test'unify = (List.map (fn (x,y) => (x,y,unify (repr_parser x) (repr_parser y)))
                  [("ΠV.(ΠX.X→Z)→Z","ΠV.W→ΠY.Y→U"),
                   ("(X→W)→X→W→Z","(X→U)→T"),
                   ("ΠX.(X→W)→X→W→Z","ΠY.(Y→U)→T"),
                   ("ΠY.(Y→U)→T","ΠX.(X→W)→X→W→Z"),
                   ("λx:X.x z","λy:V.y w"),
                   ("λx:X.x z", "λy:X.y z")])
                 handle Unify (s,(l,_)) => 
                    raise Fail ("Unify: "^s^": "^
                       (subsltostring UTF8 (List.map (fn (e,v) => (toString v,e)) l)))

val test'unify' = 
        List.map (fn (x,y,(r,s)) => (x, y, UTF8 r, subsrtostring UTF8 (unify_mgu s)))
                    test'unify

val tests = List.map emb_repr_parser ["a","ΠX.Y→X", "a b (c λx:X.x)", "(a b) c", "a b c d"]

val tests'r'UTF8 = List.map emb_UTF8 tests

val tests'r'TeX = List.map emb_TeX tests

val test' = 
   (substest UTF8 "exp" "parallel" "λx:X.(λy:X.x y) y"                    [("ΛX.λy:X.x y x'","y")],
    substest UTF8 "exp" "parallel"  "λx':X.λu:X.λv:Y.λw:X.x' (u y)"       [("λy:X.u x'","y")],
    substest UTF8 "simple_exp" "parallel" "λx':X.λu:X.λv:Y.λw:X.x' (u y)" [("λy:X.u x'","y")],
    substest UTF8 "type" "parallel" "λf:ΠY.X→Y.(λy:Y.f y) y"              [("ΠZ.Y→Z","X")],
    substest UTF8 "simple_type" "parallel" "ΠY.X→Y"                       [("ΠZ.Y→Z","X"),("ΠA.B→C","Y")],
    substest UTF8 "simple_type" "parallel" "λf:ΠY.X→Y.(λy:Y.f y) y"       [("ΠZ.Y→Z","X"),("ΠA.B→C","Y")],
    substest UTF8 "simple_type" "composed" "λf:ΠY.X→Y.(λy:Y.f y) y"       [("ΠZ.Y→Z","X"),("ΠA.B→C","Y")],
    substest UTF8 "type" "parallel" "ΛY.λf:ΠY.X→Y.(λy:X.f y) y"           [("ΠZ.Y→Z","X")],
    substest UTF8 "type" "parallel" "ΠX.Y→(ΠY.X→Y)"                       [("ΠY.X→Y","X")])
