(* This is a direct implementation of Ridge combinator parsing. See
   Parser.sml for more details and references to the original
   papers, whence most of the following code was drawn. *)

signature ParserGeneratorModuleArguments =
sig
   type semantic
   type symbol
   type results = (semantic * substring) list
   val symbolBoolDecon : (string -> bool) -> (string -> bool) -> symbol -> bool
   val symbolResultsDecon : (string -> results) -> (string -> results) -> symbol -> results
end

signature ParserGeneratorModule =
sig
   type semantic
   type symbol
   type cache = semantic Parser.cachet
   exception Semantics of string * semantic list
   val grammar_to_parser :
         (string -> substring Parser.parser) ->
         (string -> semantic) ->
         (string * semantic list -> semantic) -> cache ->
          (symbol * symbol list list) list ->
           symbol -> semantic Parser.parser
end

functor ParserGeneratorModule
   (Arguments : ParserGeneratorModuleArguments) : ParserGeneratorModule
      where type semantic = Arguments.semantic =
struct
   type semantic = Arguments.semantic
   type cache = (string * (int * semantic Parser.memo)) Parser.ml ref

   datatype symbol = datatype Arguments.symbol
   exception Semantics of string * semantic list

   open Parser
   open ParserMonad
   open ParserTerminals

   infixr **>
   infix |||
   infix >>

   val always:'a list Parser.parser =
                 fn i => [([],Parser.substr i)]
   val never = fn i:Parser.input => []

   fun then_list ([]:'a Parser.parser list) = always
     | then_list ((p::ps):'a Parser.parser list) =
                      ((p **> (then_list ps)) >> (op ::))

   fun or_list [] = never
     | or_list (p::ps) = (p ||| (or_list ps))

   fun then_list2 (semantics:string * semantic list -> semantic) = 
    fn nt => fn ps => then_list ps >> (fn xs => semantics (nt,xs))

   fun grammar_to_parser 
           p_of_tm 
          (strExn:string -> semantic) 
          (semantics:string * semantic list -> semantic) 
           cache =
      let val then_list2s = then_list2 semantics
          fun linkmp nt =
             let fun it ntno' (l as (ref Empty)) = 
                        let val ntno'' = ntno'+1
                            val mp = ref Empty
                            val m = Link((nt,(ntno'',mp)),ref Empty)
                        in
                            l := m;
                            (ntno'',mp)
                        end
                   | it ntno (ref (Link((nt',(ntno',mp)),l))) = 
                        if nt=nt' then (ntno',mp) else it ntno' l
             in it 0
             end
          fun iter cache g sym =
             fn i => Arguments.symbolResultsDecon
                (fn tm => ((p_of_tm tm) >>
                     (fn v => (strExn (Substring.string v)))) i)
                (fn nt => 
                   let val (ntno,mp) = linkmp nt cache
                       val rules = List.filter 
                                    (fn (sym,_) => Arguments.symbolBoolDecon
                                        (fn _ => false)
                                        (fn nt' => nt'=nt) sym) g
                       val alts1 =
                              (List.concat o (List.map (fn (_,b) => b)))
                              rules
                       val alts2 = List.map
                              (List.map (fn sym => iter cache g sym))
                              alts1
                       val p = or_list (List.map (then_list2s nt) alts2)
                   in memop mp (check_and_upd_lctxt ntno p) i
                   end) sym
      in
         iter cache
      end
end

signature ParserGenerator =
sig
   type semantic
   type cache = semantic Parser.cachet
   val generate_parser : (string -> substring Parser.parser) ->
         (string -> semantic) ->
         (string * semantic list -> semantic) -> cache ->
            string -> string -> semantic Parser.parser
end

signature ParserGeneratorArguments =
sig
   type semantic
   val termCons : string -> semantic
end

functor ParserGenerator
    (Arguments: ParserGeneratorArguments) : ParserGenerator =
struct
   open Parser
   open ParserMonad

   open ParserTerminals

   infixr **>
   infix |||
   infix >>

   type cache = Arguments.semantic Parser.cachet

   exception GrammarError of string

   fun assq p l =
      let fun iter [] = raise GrammarError ("Failed to associate key "^p)
            | iter ((k,v)::xs) =
                  if (k=p) then v else iter xs
      in iter l end

   fun p_of_tm tm =
      let val q = String.substring (tm,0,1)
          val stripquotes = fn tm =>
                String.substring (tm,1,(String.size tm) - 2)
      in 
         if q = "\"" then parse_a (stripquotes tm)
         else if q = "<" then parse_lws_a (stripquotes tm)
         else if q = "'" then parse_a (stripquotes tm)
         else assq tm [("?AZs?",parse_AZs),
                       ("?azAZs?",parse_azAZs),
                       ("?azs?",parse_azs),
                       ("?notsquote?",parse_notsquote),
                       ("?notdquote?",parse_notdquote),
                       ("?notgt?",parse_notgt),
                       ("?ws?",parse_ws)]
      end

   val grammar_text =
      "RULES <- RULE | RULE ?ws? RULES\n\
      \RULE <- SYM ?ws? '<-' ?ws? SYMSLIST\n\
      \SYMSLIST <- ALT | ALT ?ws? '|' ?ws? SYMSLIST\n\
      \ALT <- SYMS | <[> SEMSYMS <]> '=' <{> SEMACTS <}>\n\
      \SYMS <- SYM | SYM ?ws? SYMS\n\
      \SEMSYMS <- SEMSYM | SEMSYM ?ws? SEMSYMS\n\
      \SEMSYM <- SYM | ?azs? <=> SYM\n\
      \SEMACTS <- ?azAZs? | ?azAZs? ?ws? SEMACTS\n\
      \SYM <- '<' ?notgt? '>' | '\"' ?notdquote? '\"'\n\
      \ | \"'\" ?notsquote? \"'\" | ?AZs? | '?' ?azAZs? '?'"

   datatype symbol = TM of string * string
                   | NT of string * string

   fun symbolDecon f g =
         fn sym =>
            case sym of
                (TM (tm,_)) => f tm
              | (NT (nt,_)) => g nt

   datatype production = strProd of string
                       | strListProd of string list
                       | symProd of symbol
                       | symListProd of symbol list * string list
                       | symListListProd of (symbol list * string list) list
                       | ruleProd of symbol * (symbol list * string list) list
                       | ruleListProd of (symbol * (symbol list * string list) list) list

   exception Semantics of string * production list

   fun extractGrammar [ruleListProd e] = e
     | extractGrammar e = raise Semantics ("Bind failed",e)

   fun printParserExn sep p =
      let val _ = print sep
          val _ = case p of
                strProd s => print s
              | strListProd l => ignore (Meta.printVal l)
              | symProd s => ignore (Meta.printVal s)
              | symListProd l => ignore (Meta.printVal l)
              | symListListProd l => ignore (Meta.printVal l)
              | ruleProd l => ignore (Meta.printVal l)
              | ruleListProd l => ignore (Meta.printVal l)
      in () end

   fun parserFailHandler s es =
      let fun iter n [] = ()
            | iter n (e::es) = 
                 let val sep = if n = 0 then "" else "\n"
                 in
                    printParserExn sep e;
                    iter (n+1) es
                 end
      in
         print ("Exception: Semantics: "^s^"\n");
         iter 0 es;
         print "\n"
      end

   type semantic = Arguments.semantic

   structure MetaGrammarParser =
      ParserGeneratorModule(type semantic = production
                            type symbol = symbol
                            type results = (semantic * substring) list
                            val symbolBoolDecon = symbolDecon
                            val symbolResultsDecon = symbolDecon)

   fun semantics ps =
      case ps of
        ("RULES",[ruleProd r]) => ruleListProd[r]
      | ("RULES",[ruleProd r,_,ruleListProd rs]) => ruleListProd (r::rs)
      | ("RULE",[symProd s,_,strProd "<-",_,symListListProd l]) => ruleProd (s,l)
      | ("SYMSLIST",[symListProd sl]) => symListListProd [sl]
      | ("SYMSLIST",[symListProd sl,_,strProd "|",_,symListListProd sls]) =>
                 symListListProd (sl::sls)
      | ("SYMS",[symProd s]) => symListProd ([s],[])
      | ("SYMS",[symProd s,_,symListProd (ss,sa)]) => symListProd ((s::ss),[])
      | ("ALT",[symListProd sl]) => symListProd sl
      | ("ALT",[_,symListProd (syl,_),_,_,_,strListProd stl,_]) => symListProd (syl,stl)
      | ("SEMACTS",[strProd s]) => strListProd [s]
      | ("SEMACTS",[strProd s,_,strListProd ss]) => strListProd (s::ss)
      | ("SEMSYMS",[symProd s]) => symListProd ([s],[])
      | ("SEMSYMS",[symProd s,_,symListProd (ss,sa)]) => symListProd ((s::ss),[])
      | ("SEMSYM", [s as (symProd _)]) => s
      | ("SEMSYM", [strProd v,_,symProd s]) =>
             symProd (case s of (TM(s',_)) => TM(s',v)
                              | (NT(s',_)) => NT(s',v))
      | ("SYM", [strProd "\"",strProd s,_]) => symProd (TM("\"" ^ s ^ "\"",""))
      | ("SYM", [strProd "'",strProd s,_]) => symProd (TM("'" ^ s ^ "'",""))
      | ("SYM", [strProd "<",strProd s,_]) => symProd (TM("<" ^ s ^ ">",""))
      | ("SYM", [strProd "?",strProd s,_]) => symProd (TM("?" ^ s ^ "?",""))
      | ("SYM", [strProd s]) => symProd (NT (s,""))
      | (s,ps) => raise Semantics (s,ps)

   structure ObjectGrammarParser =
      ParserGeneratorModule(type semantic = semantic
                            type symbol = symbol
                            type results = (semantic * substring) list
                            val symbolBoolDecon = symbolDecon
                            val symbolResultsDecon = symbolDecon)

   fun call_parser parser =
      fn s =>
         let val syntax =
                 (List.map (fn (ruleListProd x,_) => x | _ => [])
                      (List.filter
                          (fn (l,ss) => Substring.size ss = 0)
                          (parser (toinput s)))) 
             fun strip_syntax l = List.map 
                  (fn (s,ls) => (s,List.map (fn (sms,_) => sms) ls)) l
         in if null syntax then [] else
            strip_syntax (hd syntax)
         end

   fun generate_parser1 parser p_of_tm strExn semantics cache grammar top = 
      let val syntax = call_parser parser grammar
      in
         if syntax = [] 
            then (Meta.printVal cache;print"\n";
                  raise Fail "Error: syntax error in grammar")
            else MetaGrammarParser.grammar_to_parser
                    p_of_tm strExn semantics cache syntax (NT (top,""))
      end

   (* Delia Smith bootstrapping ... *)
   val grammar1 = [(NT("RULES", ""),
                    [[NT("RULE", "")],
                     [NT("RULE", ""), TM("?ws?", ""), NT("RULES", "")]]),
                   (NT("RULE", ""),
                    [[NT("SYM", ""), TM("?ws?", ""), TM("'<-'", ""),
                      TM("?ws?", ""), NT("SYMSLIST", "")]]),
                   (NT("SYMSLIST", ""),
                    [[NT("SYMS", "")],
                     [NT("SYMS", ""), TM("?ws?", ""), TM("'|'", ""),
                      TM("?ws?", ""), NT("SYMSLIST", "")]]),
                   (NT("SYMS", ""),
                    [[NT("SYM", "")],
                     [NT("SYM", ""), TM("?ws?", ""), NT("SYMS", "")],
                     [TM("<[>", ""), NT("SEMSYMS", ""), TM("<]>", ""),
                      TM("'='", ""), TM("<{>", ""), NT("SEMACTS", ""),
                      TM("<}>", "")]]),
                   (NT("SEMSYMS", ""),
                    [[NT("SEMSYM", "")],
                     [NT("SEMSYM", ""), TM("?ws?", ""), NT("SEMSYMS", "")]]),
                   (NT("SEMSYM", ""),
                    [[NT("SYM", "")],
                     [TM("?azs?", ""), TM("<=>", ""), NT("SYM", "")]]),
                   (NT("SEMACTS", ""),
                    [[TM("?AZazs?", "")],
                     [TM("?AZazs?", ""), TM("?ws?", ""), NT("SEMACTS", "")]]),
                   (NT("SYM", ""),
                    [[TM("'<'", ""), TM("?notgt?", ""), TM("'>'", "")],
                     [TM("'\"'", ""), TM("?notdquote?", ""), TM("'\"'", "")],
                     [TM("\"'\"", ""), TM("?notsquote?", ""), TM("\"'\"", "")],
                     [TM("?AZs?", "")],
                     [TM("'?'", ""), TM("?azAZs?", ""), TM("'?'", "")]])]

   val parser1 =
      let val cache = ref Empty
      in
         MetaGrammarParser.grammar_to_parser
              p_of_tm strProd semantics cache grammar1 (NT ("RULES",""))
                handle _ => (Meta.printVal cache;raise Fail "parser1 fail")
      end

   val grammar2 =
         call_parser parser1 grammar_text
           handle Semantics (s,l) => 
             (parserFailHandler s l;raise Fail "Stop.")

   val parser2 =
      let val cache = ref Empty
      in
         MetaGrammarParser.grammar_to_parser
              p_of_tm strProd semantics cache grammar2 (NT ("RULES",""))
                handle _ => (Meta.printVal cache;raise Fail "parser2 fail")
      end

   val generate_parser2 = generate_parser1 parser2

   val parser3 = generate_parser2 p_of_tm strProd
                   semantics (ref Empty) grammar_text "RULES"

  fun generate_parser3 parser p_of_tm strExn semantics cache grammar top = 
      let 
          val syntax = call_parser parser grammar
      in
         if syntax = [] 
            then (Meta.printVal cache;
                  print"\n*********\n";
                  raise Fail "Error: syntax error in grammar" )
            else ObjectGrammarParser.grammar_to_parser
                    p_of_tm strExn semantics cache syntax (NT (top,""))
      end

   val generate_parser = generate_parser3 parser3
end

signature ParserModule =
sig
   type semantic
   type cache = semantic Parser.cachet
   val cache : cache
   val parse : string -> semantic list
   val showcache : (semantic -> string) -> string -> cache -> string -> unit
end
   
signature ParserModuleArguments =
sig
   type semantic
   type cache = semantic Parser.cachet
   val cache : cache
   val grammar : string
   val start : string
   val termCons : string -> semantic
   val terminals : string -> substring Parser.parser
   val semantics : string * semantic list -> semantic
end

functor ParserModule(Arguments : ParserModuleArguments) : ParserModule =
struct
      type semantic = Arguments.semantic
      type cache = Arguments.cache
      val cache = Arguments.cache
      val terminals = Arguments.terminals
      val termCons = Arguments.termCons
      val grammar = Arguments.grammar
      val start = Arguments.start
      val semantics = Arguments.semantics
      local open Parser
         structure ParserGenerator = 
               ParserGenerator(type semantic = semantic
                               val termCons = termCons)

         fun parse_grammar grammar start = 
              ParserGenerator.generate_parser
                   terminals termCons semantics cache grammar start

         fun get_nts nts l =
            case l of
               (ref(Link((nt,(ntno,ref(Link(((ctxt,n'),r),l)))),l'))) =>
                   let val ctxtb = ctxt
                   in get_nts ((ntno,nt)::nts) l'
                   end
             | _ => rev nts

         fun pad l c n s =
            let fun iter r 0 = r
                  | iter r n = iter (if l then c^r else r^c) (n-1)
                val sl = UTF8.size s
            in if sl < n then iter s (n - sl) else s
            end

         val padl = pad true
         val padr = pad false
         val spadl = padl " "
         val spadr = padr " "

         fun maxstr n s =
            let val l = String.size s
            in if l > n then String.substring (s,0,n) else s end

         fun ppexpsublist printer =
            let fun iter [] = ()
              | iter ((e,ss)::ps) = 
                  let val s = String.toString (Substring.string ss)
                      val _ = print ("\n  [")
                      val _ = print (""^(printer e)^"")
                      val _ = print (", \""^(maxstr 50 s)^"\"]")
                  in 
                     print (if not (null ps) then "," else "");
                     iter ps
                  end
             in iter
             end

         fun assq m p l =
            let fun iter [] = raise Fail ("Grammar error assq:"^m)
                  | iter ((k,v)::xs) =
                        if (k=p) then v else iter xs
            in iter l end

         fun ppcache printer nts estr = 
            let val ntsz = List.foldr 
                           (fn ((_,s),m) =>
                              let val n = String.size s
                              in if n > m then n else m end) 0 nts
                fun iter m (ref(Link((nt,(ntno,ref(Link(((ctxt,(i',n')),r),l)))),l'))) =
                    let val lr = length r
                        val esz = String.size estr
                        fun istr n = Int.toString n
                        fun pctxt _ [] = ()
                          | pctxt m ((ntno,n)::ctxt) =
                             let val nt = assq "ntno" ntno nts
                             in print ("\n   "^
                                 (spadl 4 (istr m))^" "^
                                 (spadl 3 (istr ntno))^" "^(spadr ntsz nt)^
                                 (spadl 7 ("("^(istr n)^")"))^" = "^
                          (("\""^(maxstr 50 (String.toString (String.substring(estr,i',n))))^"\"")));
                                pctxt (m+1) ctxt
                             end
                        val sep = if m > 1 then "\n" else ""
                    in
                       if true orelse lr > 0 then 
                         (print(sep^" "^nt^" ("^","^(istr n')^") = \""^
                             (maxstr 50 (String.toString (String.substring(estr,i',n')))^"\""));
                          ppexpsublist printer (rev r);
                          pctxt 1 ctxt)
                          else ();
                       iter (m+1) l'
                    end
                  | iter _ _ = ()
               in
                  iter 1
               end
      in
         val parser = parse_grammar grammar start

         fun parse s =
                 (List.map (fn (x,_) => x)
                   (List.filter
                       (fn (l,ss) => Substring.size ss = 0)
                        (parser (Parser.toinput s))))

         fun showcache (printer:semantic -> string) suffix mp e =
                 ppcache printer (get_nts [] mp) (e^suffix) mp
      end
end
