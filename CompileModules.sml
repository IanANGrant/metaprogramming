load "Process";

local
   val arglist = List.foldr (fn (x,r) => x^(if r = "" then "" else " ")^r) ""
   val arglistc = List.foldr (fn (x,r) => x^(if r = "" then "" else ",")^r) ""
   val modsl = arglistc o (List.map (fn s => "\""^s^"\""))
   val mods = arglist o (List.map (fn s => s^".ui"))
   val opts = arglist o (List.map (fn s => "-"^s))

   fun compile modlist optlist file =
      let open Process
          val cmd = "mosmlc -c "^(arglist [mods modlist,opts optlist,file])
          val _ = print ("\""^cmd^"\"")
      in ignore (system cmd);
         print "\n"
      end

in
   fun compToplevelHere modlist file =
      let val cmd = "Meta.compileToplevel ["^(modsl modlist)^"] \""^file^"\""
          val _ = print (""^cmd^"")
          val _ = Meta.orthodox()
      in Meta.compileToplevel modlist file;
         print "\n"
      end

   fun compStructureHere modlist file =
      let val cmd = "Meta.compileStructure ["^(modsl modlist)^"] \""^file^"\""
          val _ = print (""^cmd^"")
          val _ = Meta.orthodox()
      in Meta.compileStructure modlist file;
         print "\n"
      end

   val allhere = true

   fun compToplevel modlist file =
      if allhere then compToplevelHere modlist file
                 else compile modlist ["orthodox","toplevel"] file

   fun compStructure modlist file =
      if allhere then compStructureHere modlist file
                 else compile modlist ["orthodox","structure"] file

   fun rebuild () =
      let
   val _ = compToplevel [] "AbstractExp.sml"
   val _ = compToplevel [] "AbstractBind.sml"
   val _ = compToplevel [] "AbstractInfixop.sml"
   val _ = compToplevel [] "AbstractPat.sml"
   val _ = compToplevel [] "AbstractAtype.sml"
   val _ = compToplevel [] "AbstractStr.sml"
   val _ = compToplevel [] "AbstractRepr.sml"
   val _ = compToplevel [] "AbstractReprRepr.sml"

   val abstract_deps = ["AbstractExp","AbstractBind","AbstractInfixop","AbstractPat",
                        "AbstractAtype","AbstractStr","AbstractRepr","AbstractReprRepr"]

   val _ = compToplevel abstract_deps "Induction.sml"

   val _ = compToplevel ["Induction"] "Deconstructor.sml"
   val _ = compToplevel ["Induction"] "Constructor.sml"

   val _ = compToplevel ["Induction","Constructor"] "ReprPrinters.sml"
   val _ = compToplevel ["Induction","Constructor","Deconstructor"] "ReprPrinter.sml"

   val systemF_deps = abstract_deps@["Induction","Constructor","Deconstructor",
                                     "ReprPrinters","ReprPrinter"]

   val _ = compStructure systemF_deps "SystemF.sig"
   val _ = compStructure systemF_deps "SystemF.sml"

   val _ = compToplevel ["Induction","Constructor","Deconstructor"] "ReprEmbedding.sml"
   val _ = compToplevel ["Induction","Constructor","Deconstructor"] "ReprEmbedding2.sml"

   val _ = compToplevel ["Induction","Deconstructor"] "Variables.sml"
   val _ = compToplevel ["Induction","Deconstructor"] "OccursCheck.sml"
   val _ = compToplevel ["Induction","Deconstructor"] "BoundVariables.sml"

   val _ = compToplevel [] "Semantics.sml"
   val _ = compToplevel ["Induction","Deconstructor","Constructor","Semantics"] "MapJoinSemantics.sml"

   val _ = compToplevel [] "SubsConvention.sml";
   val _ = compToplevel ["Semantics","Induction","SubsConvention"] "SimpleSubs.sml";
   val _ = compToplevel ["Induction","BoundVariables","Deconstructor"] "FreeVariables.sml"

   val _ = compToplevel ["Induction","Deconstructor","Variables",
                         "Semantics","SubsConvention"] "Barendregt.sml"
   val _ = compToplevel ["Induction","Deconstructor","Variables", "Semantics"] "SubsSemantics.sml"
   val _ = compToplevel ["Induction","Deconstructor",
                         "Constructor","FreeVariables","Barendregt","SubsConvention",
                         "Semantics","SubsSemantics","SimpleSubs"] "Substitute.sml"
   val _ = compToplevel ["Induction","Constructor","Deconstructor",
                         "Substitute","Semantics"] "UnifySemantics.sml"
   val _ = compToplevel ["Induction","Deconstructor"] "OccursCheck.sml"
   val _ = compToplevel ["Induction","Constructor","Deconstructor",
                         "Substitute","Semantics","UnifySemantics","OccursCheck"] "Unification.sml"
   val _ = compToplevel ["Induction","Constructor","Deconstructor","Unification",
                         "Semantics","MapJoinSemantics","SubsSemantics","SubsConvention",
                         "SimpleSubs","FreeVariables","Substitute","OccursCheck",
                         "ReprEmbedding","UnifySemantics","Unification"] "Unifier.sml"
   val _ = compToplevel ["Induction","Constructor","Deconstructor","Unification",
                         "Semantics","MapJoinSemantics","SubsSemantics","SubsConvention",
                         "SimpleSubs","FreeVariables","Substitute","OccursCheck",
                         "ReprEmbedding","UnifySemantics","Unification"] "Matcher.sml"
   val _ = compToplevel ["Induction","FreeVariables","Semantics"] "TypeCheck.sml"

   val _ = compToplevel ["Induction","Constructor","Deconstructor"] "Unit.sml"

   val _ = compStructure [] "UTF8.sig"
   val _ = compStructure [] "UTF8.sml"
   val _ = compStructure [] "Parser.sig"
   val _ = compStructure [] "Parser.sml"
   val _ = compStructure [] "Parser2.sig"
   val _ = compStructure [] "Parser2.sml"
   val _ = compStructure [] "ParserMonad.sig"
   val _ = compStructure [] "ParserMonad.sml"
   val _ = compStructure [] "ParserTerminals.sig"
   val _ = compStructure [] "ParserTerminals.sml"
   val _ = compStructure [] "TerminalParsers.sig"
   val _ = compStructure [] "TerminalParsers.sml"
   val _ = compToplevelHere ["ParserMonad","ParserTerminals"] "SemanticParserModule.sml"
   val _ = compToplevelHere ["Int","ListPair","Parser","SemanticParserModule",
                                 "Induction", "Constructor",
                                 "Deconstructor"] "ProtoParser.sml"
   val _ = compStructure [] "TextFile.sig"
   val _ = compStructure ["TextIO","String"] "TextFile.sml"
   val _ = compToplevel ["Induction","Deconstructor","Constructor",
                         "ProtoParser","ReprPrinter","ReprPrinters"] "Syntax.sml"
   in print "Done\n" end
end
