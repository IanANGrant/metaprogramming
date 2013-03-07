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
in
   val represent = UnitM.unit

   val toString = Induction.dec_repr_str Induction.Repr.prt_repr
   val UTF8 = toString o Syntax.UTF8Printer.print
   val TeX = toString o Syntax.TeXPrinter.print
   val sem = Induction.reprToString

   val emb_sem = ReprEmbedding.reprToString

   val emb_parser_ = EmbeddedSyntax.repr_parser_

   val emb_repr_parser = EmbeddedSyntax.repr_parser
   val emb_UTF8 = toString o EmbeddedSyntax.UTF8Printer.print

   val repr_parser = Syntax.repr_parser

   val rep_str = Induction.Repr.rep_str o Induction.Str.rep_string
end

val smallt = emb_repr_parser ("ΛY.λx:Y.x")
val smallt' = (emb_UTF8 smallt, emb_sem smallt)

val reprt = emb_repr_parser ("ΛY.λx:⟪ΠX.X⟫.⟪ΛX.Y ⟪Ω⟫⟫")
val reprt' = (emb_UTF8  reprt, emb_sem reprt)
val metat = emb_repr_parser ("λ⟦λ⌜v⌝:⌜T⌝.⌜e⌝ ⌜v⌝⟧:X.⟦λ⌜v⌝:⌜T⌝.f ⌜e⌝ ⌜v⌝⟧")
val metat' = (emb_UTF8  metat, emb_sem metat)
val metat2 = emb_repr_parser ("｟w.m (w.m g)｠")
val metat2' = (emb_UTF8  metat2, emb_sem metat2)
val metat3 = emb_repr_parser ("⌜e⌝ ⌜v⌝")
val metat3' = (emb_UTF8  metat3, emb_sem metat3, sem metat3)

(* val letpair = λ⟦〈⌜p⌝·⌜ps⌝〉=⌜e'⌝.⌜e⌝⟧.
   λv=newvar e.⟦λ⌜v⌝=⌜e'⌝.⌜⟦λ⌜p⌝=π1 ⌜v⌝,⌜ps⌝=π2 ⌜v⌝.⌜⟦⌜e⌝⟧⌝⟧⌝⟧ *)
