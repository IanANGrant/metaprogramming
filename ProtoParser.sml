signature ProtoParser =
sig
    type repr
    val grammar : string
    val start : string
    val cache : repr Parser.cachet
    val parser : string -> (repr * substring) list
    val parser_ : repr Parser.parser
    val printer : (repr -> repr -> repr) -> repr -> string * repr -> repr
 (* val printer : ('a -> repr -> 'a) -> 'a -> string * repr -> 'a *)
end

functor ProtoParser
  (type repr
   val grammar : string
   val start : string
   val terminals : string -> substring Parser.parser
   val cache : repr Parser.cachet
   structure Induction : Induction
       where type repr = repr
   structure Deconstructor : Deconstructor
       where type repr = repr
   structure Constructor : Constructor
       where type repr = repr
   ) :> ProtoParser
   where type repr = repr =
struct
   type repr = repr
local

   val prodListToString = Induction.reprListToString
   val prodToString = Induction.reprToString

   open Induction

   open Parser
   open ParserMonad
   open ParserTerminals

   infixr **>
   infix |||
   infix >>

datatype symbol = TM of string * string
                | NT of string * string

fun symbolDecon f g =
      fn sym =>
         case sym of
             (TM (tm,_)) => f tm
           | (NT (nt,_)) => g nt

fun printSyms ss = 
   let fun iter r [] = r
         | iter r (sym::syms) = 
            let val st = case sym of NT(s,s') => "NT("^s^","^s'^")"
                                   | TM(s,s') => "TM("^s^","^s'^")"
                val s = if r = "" then "" else ", "
            in iter (r^s^st) syms end
        val str = iter "" ss
   in str
   end

datatype lexp = Apply of string * lexp
              | Nil

val lexpToList =
   let fun iter r Nil = rev r
         | iter r (Apply(s,ss)) = iter (s::r) ss
   in iter []
   end

datatype production = strProd of string
                    | lexpProd of lexp
                    | symProd of symbol
                    | symListProd of symbol list * lexp
                    | symListListProd of (symbol list * lexp) list
                    | ruleProd of (symbol * string) * (symbol list * lexp) list
                    | ruleListProd of ((symbol * string) * (symbol list * lexp) list) list

type cache = production Parser.cachet
exception Syntax of string * cache
exception Semantics of string * production list

fun extractGrammar [ruleListProd e] = e
  | extractGrammar e = raise Semantics ("Bind failed",e)

fun printParserExn sep p =
   let val _ = print sep
       val x = case p of
             strProd s => print s
           | lexpProd l => ignore (Meta.printVal l)
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

val meta_grammar =
      "TOP <- RULES ?optws? ';'\n\
      \RULES <- RULE | RULE ?ws? RULES\n\
      \RULE <- SYM <:> ?semacts? ?ws? '<-' ?ws? SYMSLIST\n\
      \SYMSLIST <- ALT | ALT ?ws? '|' ?ws? SYMSLIST\n\
      \ALT <- SYMS | SEMS\n\
      \SEMS <-  <[> SEMSYMS <]> <=> <{> SEMACTS ?optws? '}'\n\
      \SYMS <- SYM | SYM ?ws? SYMS\n\
      \SEMSYMS <- SEMSYM | SEMSYM ?ws? SEMSYMS\n\
      \SEMSYM <- SYM | ?vars? <=> SYM\n\
      \SEMACTS <- SEMACT | SEMACT ?ws? SEMACTS\n\
      \SEMACT <- ?semacts? | ?semacts? '*' | ?semacts? '.' SEMACT\n\
      \SYM <- '<' ?notgt? '>' | '\"' ?notdquote? '\"'\n\
      \ | \"'\" ?notsquote? \"'\" | ?AZs? | '?' ?azAZs? '?'"

fun semantics ps =
   case ps of
     ("TOP",[l as ruleListProd _,_,_]) => l
   | ("RULES",[ruleProd r]) => ruleListProd[r]
   | ("RULES",[ruleProd r,_,ruleListProd rs]) => ruleListProd (r::rs)
   | ("RULE",[symProd s,_,strProd s',_,strProd "<-",_,symListListProd l]) =>
              ruleProd ((s,s'),l)
   | ("SYMSLIST",[symListProd sl]) => symListListProd [sl]
   | ("SYMSLIST",[symListProd sl,_,strProd "|",_,symListListProd sls]) =>
              symListListProd (sl::sls)
   | ("SYMS",[symProd s]) => symListProd ([s],Nil)
   | ("SYMS",[symProd s,_,symListProd (ss,sa)]) => symListProd ((s::ss),sa)
   | ("ALT",[l as (symListProd _)]) => l
   | ("SEMS",[_,symListProd (syl,_),_,_,_,lexpProd se,_,_]) =>
              symListProd (syl, se)
   | ("SEMACTS",[strProd s]) => lexpProd (Apply(s,Nil))
   | ("SEMACTS",[strProd s,_,lexpProd ss]) => lexpProd (Apply (s,ss))
   | ("SEMACT",[s as (strProd _)]) => s
   | ("SEMACT",[strProd s,_,strProd ss]) => strProd (s^"."^ss)
   | ("SEMACT",[strProd s,strProd s']) => strProd (s^s')
   | ("SEMSYMS",[symProd s]) => symListProd ([s],Nil)
   | ("SEMSYMS",[symProd s,_,symListProd (ss,sa)]) =>
               symListProd ((s::ss),sa)
   | ("SEMSYM", [s as (symProd _)]) => s
   | ("SEMSYM", [strProd v,_,symProd s]) =>
          symProd (case s of (TM(s',_)) => TM(s',v)
                           | (NT(s',_)) => NT(s',v))
   | ("SYM", [strProd "\"",strProd s,_]) => symProd (TM("\"" ^ s ^ "\"",""))
   | ("SYM", [strProd "'",strProd s,_]) => symProd (TM("'" ^ s ^ "'",""))
   | ("SYM", [strProd "<",strProd s,_]) => symProd (TM("<" ^ s ^ ">",""))
   | ("SYM", [strProd "?",strProd s,_]) => symProd (TM("?" ^ s ^ "?",""))
   | ("SYM", [strProd s]) => symProd (NT (s,""))
   | (s,ps) => (Meta.printVal (s,ps);raise Semantics (s,ps))

val parse_vars = 
   let val lpred = 
        fn c => inrange "a" "z" c
        val rpred =
        fn c => lpred c orelse inlist ["_","'"] c
                        orelse inrange "A" "Z" c
                        orelse inrange "0" "9" c
   in
      (parse_one_ilws lpred **>
       (parse_while "vars" rpred))
         >> (fn (s1,s2) =>
               (Substring.span(s1,s2)))
   end

val parse_semacts = 
   let val lpred = 
        fn c => inrange "a" "z" c
         orelse inrange "A" "Z" c
        val rpred =
        fn c => lpred c orelse inlist ["_","'"] c
                        orelse inrange "0" "9" c
   in
      (parse_one_ilws lpred **>
       (parse_while "semacts" rpred))
         >> (fn (s1,s2) =>
               (Substring.span(s1,s2)))
   end

exception GrammarError of string

fun assq p l =
   let fun iter [] = raise GrammarError ("No terminal with key "^p)
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
                    ("?vars?",parse_vars),
                    ("?semacts?",parse_semacts),
                    ("?notsquote?",parse_notsquote),
                    ("?notdquote?",parse_notdquote),
                    ("?notgt?",parse_notgt),
                    ("?optws?",parse_optws),
                    ("?ws?",parse_ws)]
   end

structure GrammarParser =
  ParserModule
     (type semantic = production
      type cache = cache
      val cache = ref Empty
      val grammar = meta_grammar
      val start = "TOP"
      val termCons = strProd
      val terminals = p_of_tm
      val semantics = semantics)

fun parser s =
   let val _ = GrammarParser.cache := Empty
       val r = GrammarParser.parse (s^";")
       val rules =
             if null r 
                then (GrammarParser.showcache (fn _ => "") ";" 
                      GrammarParser.cache s;ruleListProd [])
                else hd r
       val grammar = case rules of (ruleListProd grammar) => grammar
                                 | _ => raise Fail "parser: Can't extract rules"
   in grammar
   end

fun nttype grammar =
   fn nt =>
      let fun iter [] = raise Fail ("No nonterminal "^nt)
            | iter (((NT(nt',""),t),_)::rs) =
                  if nt = nt' then t else iter rs
            | iter _ = raise Fail "Bad rule"
      in
         iter grammar
      end

val mem = fn x => List.exists (fn x' => x=x')

fun collect_args grammar ignore_terms ignoreliterals = List.foldl
  (fn (NT (p as (nt,v)),(r,n,e)) => 
          ((n,if v = ""
                 then "x"
                 else v,nt,nttype grammar nt)::r,n+1,e+1)
    | (TM (p as (tm,v)),(r,n,e)) => 
        let val nttype = if String.substring (tm,0,1) = "?"
                            then "terminal"
                            else "literal"
            val result = if not (mem tm ignore_terms) 
                            andalso (not (nttype = "literal")
                                     orelse not ignoreliterals)
                            then ((n,v,tm,nttype)::r,n+1,e+1)
                            else (r,n+1,e)
        in result end)
  ([],1,0)

val ignore_terms = ["?optws?","?ws?"]
fun args grammar = collect_args grammar ignore_terms true

fun alts grammar =
   fn nt =>
      let fun iter r [] = r
            | iter r (((NT(nt',""),t),l)::rs) = 
                let val r' = if nt = nt' then l::r else r
                in iter r' rs end
            | iter r _ = raise Fail "Bad rule"
      in
         List.concat (rev (iter [] grammar))
      end

val nts = List.map (fn ((NT(nt,""),t),_) => (nt,t)
                     | _ => raise Fail "Bad grammar")

val ntnts = List.map (fn ((NT(nt,""),t),_) => nt
                     | _ => raise Fail "Bad grammar")

fun extract_semantics grammar = 
   let fun arglist syms =
          let val (l,_,_) = args grammar syms
          in rev l end
       fun nonterminal (nt,t) =
             List.map
               (fn (syms,lexp) => 
                   let val lexp' = if lexp = Nil
                                      then Apply("id", Apply("x", Nil))
                                      else lexp
                   in (nt,t,syms,lexp',arglist syms)
                   end) 
               (alts grammar nt)
   in
      List.map nonterminal (nts grammar)
   end

val compile_constructors =
   let fun iter nt t m r [] = ((NT(nt,""),t),rev r)
         | iter _ _ m r ((nt,t,syms,lexp,args)::cs) =
              let val nargs = length args
                  val nargss = Int.toString nargs
                  val ms = Int.toString m
                  val ntn = nt^"("^ms^")"
                  fun lexpMap f = 
                     let fun iter r (Apply(s,es)) = iter ((f s)::r) es
                           | iter r Nil = rev r
                     in iter [] end
                  fun varidx args = fn v =>
                      let val n = List.foldl 
                            (fn ((n,v',_,_),e) =>
                                if v=v' then if e=0 then n else raise Fail 
                                               ("Multiple "^v^" arguments in "^ntn)
                                        else e) 0 args
                      in if n = 0
                            then NONE
                            else SOME (n-1)
                      end
                  val varidxargs = varidx args
                  fun assq p l =
                      let fun iter [] = NONE
                            | iter ((k,v)::xs) =
                              if (k = p) then SOME v else iter xs
                      in iter l end
                  fun compile_null () =
                      fn f => fn _ => f (t,nt,[])
                  fun deref t n l = 
                        if n < (length l) andalso n >= 0
                           then List.nth (l,n)
                           else raise Fail ("compile: bad "^t^".id args ["^
                                                (prodListToString l)^"]")
                  fun compile_one () =
                      let val (n,t') = case args of
                                          [(n,_,_,t')] => (n,t')
                                        | _ => raise Fail "get_constructors: \
                                              \an impossible thing happened."
                         val func = if t = t' 
                                then (fn f => fn l => deref t (n-1) l)
                                else (fn f => fn l => f (t,t',[deref t (n-1) l]))
                      in func end
                  fun splitpair s =
                      let val ts =  String.tokens (fn c => c = #".") s
                      in case ts of [cn] => ("",cn)
                            | [t,cn] => (t,cn)
                            | _ => raise Fail ("compile_constructors: bad constructor "^s)
                      end
                  fun compile lexp = 
                      let val (t,c,e) =
                             case lexp of 
                                  Apply(s,Nil) => (t,s,Nil)
                                | Apply(s,e) => (t,s,e)
                                | Nil => (raise Fail ("Bad lexp Nil in "^ntn))
                      in if e = Nil
                            then fn f => fn l => f (t,c,[])
                             else  if c = "id" then
                                           case e of (Apply (v,Nil)) =>
                                               (fn f => fn l =>
                                                   (case varidxargs v of
                                                     NONE => raise Fail
                                                              ("compile: no argument: "^t^".id")
                                                   | SOME n => deref t n l))
                                               | _ => raise Fail ("compile: bad args "^t^".id") 
                            else fn f => fn l => 
                               f (t,c, lexpMap (fn v =>
                                           let val (t,c) = splitpair v
                                           in case varidxargs v of
                                                  NONE => f (t,c,[])
                                                | SOME n => deref t n l
                                           end) e)
                      end
                  val f = case (lexp,nargs) of
                            (Nil,0) => compile_null ()
                          | (Nil,1) => compile_one ()
                          | (Nil,_) => raise Fail ("Non-unique argument ("^
                                          (nargss)^" possible) in "^ntn)
                          | (e,_) => compile e
              in iter nt t (m+1) ((m,syms,lexp,f)::r) cs
              end
   in
      iter "" "" 1 []
   end

val semantics = (List.map compile_constructors) o extract_semantics

fun strip_grammar semantics =
   List.map (fn ((nt as (NT_),_),l) =>
                 let val l' = List.map (fn (_,ss,_,f) => (ss,f)) l
                 in (nt,l') end) semantics

fun mem x' = List.exists (fn x => x = x')

   val always:'a list Parser.parser =
                 fn i => [([],Parser.substr i)]

   val never = fn i:Parser.input => []

   fun then_list ([]:'a Parser.parser list) = always
     | then_list ((p::ps):'a Parser.parser list) =
                      ((p **> (then_list ps)) >> (op ::))

   fun or_list [] = never
     | or_list (p::ps) = (p ||| (or_list ps))

   fun then_list2 semantics f nt = 
     fn ps => then_list ps >> (fn xs => (f semantics xs))

   fun grammar_to_parser
           p_of_tm
          (mk_terminal:string -> repr)
          (semantics:string * string * repr list -> repr)
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
             fn i => symbolDecon
                (fn tm => ((p_of_tm tm) >>
                     (fn v => (mk_terminal (Substring.string v)))) i)
                (fn nt => 
                   let val (ntno,mp) = linkmp nt cache
                       val rules = List.filter 
                                    (fn (sym,_) => symbolDecon
                                        (fn _ => false)
                                        (fn nt' => nt'=nt) sym) g
                       val alts1 =
                              (List.concat o (List.map (fn (_,b) => b)))
                              rules
                       val alts2 = List.map
                              (fn (l,f) => (f,List.map (fn sym => iter cache g sym) l))
                              alts1
                       val p = or_list (List.map (fn (f,l) => then_list2s f nt l) alts2)
                   in memop mp (check_and_upd_lctxt ntno p) i
                   end) sym
      in
         iter cache
      end

local
   open Parser
         fun get_nts nts l =
            case l of
               (ref(Link((nt,(ntno,ref(Link(((ctxt,(i',n')),r),l)))),l'))) =>
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
                          | pctxt m ((ntno,(i,n))::ctxt) =
                             let val nt = assq "ntno" ntno nts
                             in print ("\n   "^
                                 (spadl 4 (istr m))^" "^
                                 (spadl 3 (istr ntno))^" "^(spadr ntsz nt)^
                                 (spadl 7 ("("^(istr i)^","^(istr n)^")"))^" = "^
                          (("\""^(maxstr 50 (String.toString 
                                     (String.substring(estr,i,n))))^"\"")));
                                pctxt (m+1) ctxt
                             end
                        val sep = if m > 1 then "\n" else ""
                    in
                       if true orelse lr > 0 then 
                         (print(sep^" "^nt^" ("^(istr i')^","^(istr n')^") = \""^
                             (maxstr 50 (String.toString
                                      (String.substring(estr,i',n')))^"\""));
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
         fun showcache (printer) suffix mp e =
                 ppcache printer (get_nts [] mp) (e^suffix) mp
      end

fun idclosure semantics =
   let fun altsfn nt t =
            fn (n,ss,lexp,_) =>
               let val (cn,argps) =
                   case lexp of
                       Apply(cn,ps) => (cn,lexpToList ps)
                     | _ => ("",[])
               in (nt,t,cn,argps,ss)
               end
       fun constrs semantics filter = 
          let val map = (fn ((NT(nt,_),t),alts) =>
                         List.filter filter (List.map (altsfn nt t) alts)
                          | ((TM(tm,_),_),_) => 
                               raise Fail ("constrs: Nonterminal terminal "^tm))
          in  List.concat (List.map map semantics)
          end
       val all_constrs = constrs semantics (fn _ => true)
       fun inctxt nt ctxt = List.exists (fn (nt',_) => nt'=nt) ctxt
       fun enum_tree f g r =
           let fun enum_branches ctxt r branches =
               let fun iter branches r [] _ = r
                     | iter branches r ((branch as (nt,_,cn,_,ss))::rs) nt' =
                          let val ctxt' = if cn = "id" then (nt,ss)::ctxt else ctxt
                              val branches' = branch::branches
                              val r' =
                                  if nt' = nt
                                     then enum_leaves ctxt' 
                                            (rs@(rev branches'))
                                               r branch
                                     else r
                          in iter branches' r' rs nt'
                          end
               in iter [] r branches
               end
           and enum_leaves ctxt branches r (branch as (nt,t,cn,vs,ss)) =
               let fun iter r [] = f ctxt branch r
                     | iter r (s::ss') =
                        let val r' =
                           case s of
                              NT(nt,_) =>
                                 if cn = "id"
                                    then if not (inctxt nt ctxt)
                                            then enum_branches ctxt
                                                  (g s ctxt branch r)
                                                   branches nt
                                            else r
                                    else g s ctxt branch r
                            | TM _ => g s ctxt branch r
                        in iter r' ss'
                        end
               in iter r ss
               end
          in enum_branches [] r all_constrs
          end
       fun constrs_orlist ctxt (nt,t,cn,vs,ss) (cstrs,symls,r) =
          let fun contextsyms ctxt nt =
              let fun iterouter r nt [] = r
                    | iterouter r nt ((nt',syms)::ps) = 
                          if null ps
                             then (rev syms)@r
                             else iterinner r ps syms
                  and iterinner r ps [] =  r
                    | iterinner r ps (sym::syms) =
                         case sym of
                             NT (nt,_) => iterinner (iterouter r nt ps) ps syms
                           | TM (s,s') => iterinner (sym::r) ps syms 
              in rev (iterouter [] nt (rev ctxt)) end
          in
             if not (cn = "id")
                then let val syms = contextsyms ((nt,rev r)::ctxt) nt
                     in ((nt,t^"."^cn,vs,syms)::cstrs,(nt,t,cn,vs,syms)::symls,[]) end
                else (cstrs,symls,[])
          end
       fun constrs_thenlist s ctxt (nt,t,cn,vs,ss) (cstrs,symls,r) =
            let val r' = if cn = "id" then r else s::r
            in (cstrs,symls,r') end
   in fn nt =>
      let val (r,_,_) = enum_tree constrs_orlist constrs_thenlist ([],[],[]) nt 
      in  r end
   end

fun idcl semantics nt =
   let val idc = rev (idclosure semantics nt)
       fun firstc tm  = String.substring (tm,0,1)
       val stripquotes = fn tm => if (String.size tm) <= 1 
                                     then raise Fail ("idcl: striquotes: index "^tm)
                                     else String.substring (tm,1,(String.size tm) - 2)
   in List.map (fn (_,cn,vars,syms) => ((List.map 
                    (fn (TM(tm,v)) =>
                          let val tm' = stripquotes tm
                              val fc = firstc tm
                          in if v = "" then (if fc="?" then tm else tm) else v^"="^tm end
                 | (NT(nt,v)) => (v^"="^nt)) syms),cn::vars)) idc
   end

exception UnifyFailed of string

val debug_unification = false

fun unify syntax semantics =
   let val cstrs = idclosure semantics
       val constructor_list =
               List.map (fn ((NT(nt,_),_),_) => (nt,cstrs nt)
                          | _ => raise Fail "unify: bad grammar") syntax
       fun assq p l =
           let fun iter [] = NONE
                 | iter ((k,v)::xs) =
                   if (k = p) then SOME v else iter xs
           in iter l end       
       fun unify nt c =
              case assq nt constructor_list of
                 SOME l => (List.filter (fn (_,cn,_,_) => (cn = c)) l)
               | NONE => []
       fun args_idx v l =
           let fun iter _ [] = NONE
                 | iter n (s::ss) =
                     if v=s then SOME n
                            else iter (n+1) ss
           in iter 1 l end
       fun variable v l =
           let fun iter [] = false
                 | iter (s::ss) =
                        let val v' = case s of 
                                         NT(_,v') => v'
                                       | TM(_,v') => v'
                        in if v=v' then true
                                   else iter ss
                        end
           in iter l end
       fun unify_constants syms args prods =
           let fun splitpair s =
                   let val ts =  String.tokens (fn c => c = #".") s
                   in case ts of [cn] => ("",cn)
                                | [t,cn] => (t,cn)
                                | _ => raise Fail ("unify: unify_constants: bad constant "^s)
                   end
              fun deref t c n l = 
                     if n < (length l) andalso n >= 0
                        then List.nth (l,n)
                        else raise Fail ("unify_constants: bad "^t^"."^c^" args ["^
                                            (prodListToString l)^"]")
               fun iter _ [] = ()
                 | iter n (a::args') = 
                     if variable a syms
                        then iter (n+1) args'
                        else let val (argp as (t,c)) = splitpair a
                                 val prod = deref t c (n-1) prods
                                 val pdeconstr = (fn p => fn f => f (Deconstructor.deconstr p))
                                 fun constseq prod (t,c) =
                                          pdeconstr prod (fn
                                             (t',c',[]) => t=t' andalso c=c'
                                           | _ => false)
                             in if not (constseq prod argp)
                                   then raise UnifyFailed ("constants: "^a)
                                   else iter (n+1) args'
                             end
           in iter 1 args
           end
       fun mk_deconstructor formatter output state = fn nt =>
           let val corecur = formatter output
               fun isliteral tm = not ((String.size tm) > 1
                          andalso (String.substring (tm,0,1) = "?"))
               val stripquotes = fn tm =>
                     if (String.size tm) <= 1
                         then raise Fail ("mk_deconstructor: striquotes: index "^tm)
                         else String.substring (tm,1,(String.size tm) - 2)
               val stripquotes = fn tm =>
                     String.substring (tm,1,(String.size tm) - 2)
               fun fromString tm = Induction.Repr.rep_str(Induction.Str.fromMLString tm)
               fun apply (nt',cn',args,syms) prods =
                   if (length args) <> (length prods)
                      then raise Fail ("arity: "^nt'^":"^cn'^" "^
                                       (prodListToString prods))
                      else
                   let val _ = if debug_unification
                                   then print ("apply: "^nt'^":"^cn'^" "^
                                               (prodListToString prods)^"\n")
                                   else ()
                       fun iter _ state [] = state
                         | iter prods state (s::ss) =
                              let fun argref nt v =
                                  let val n =
                                        case args_idx v args of
                                            NONE => raise Fail 
                                                  ("No index for variable "^
                                                    v^"("^nt^") in "^nt'^":"^cn')
                                          | SOME n => n
                                  in corecur state (nt,List.nth (prods,n-1))
                                      handle Subscript => (Meta.printVal args;
                                               raise Fail
                                                 ("Subscript: mk_deconstructor "^v^"("^nt^") "^
                                                  (Int.toString (n-1))^"  "^nt'^":"^cn'^
                                                  " "^prodListToString prods))
                                  end
                              in
                                 (case s of
                                      NT(nt'',v) => iter prods (argref nt'' v) ss
                                    | TM(tm,txt) =>
                                       let val _ = if debug_unification
                                                      then print ("apply: iter: tm='"^tm^
                                                             "' txt='"^txt^"' "^
                                                             (prodListToString prods)^"\n")
                                                      else ()
                                           val state' =
                                               if isliteral tm
                                                  then corecur state 
                                                     ("literal", fromString (stripquotes tm))
                                               else if tm = "?ws?"
                                                       then (corecur state
                                                          ("terminal", fromString " "))
                                                               handle Subscript =>
                                                                 raise Fail 
                                                                    "apply: iter: Subscript exn"
                                               else if tm = "?optws?" then state
                                               else argref (stripquotes tm) txt
                                       in iter prods state' ss
                                       end)
                              end
                   in
                      unify_constants syms args prods;
                      iter prods state syms
                   end
           in
              fn (t,c,prods) =>
                   let val _ = if debug_unification
                                   then print ("mk_constructor: "^nt^":"^t^"."^c^" "^
                                                     (prodListToString prods)^"\n")
                                   else ()
                       fun iter [] = raise UnifyFailed (nt^": "^t^"."^c^" "^
                                                     (prodListToString prods))
                         | iter (alt::alts) = apply alt prods
                                handle UnifyFailed s => 
                                       if null alts
                                          then raise UnifyFailed (t^"."^c^": ("^s^")")
                                          else iter alts
                   in
                      iter (unify nt (t^"."^c))
                   end
           end
           val pdeconstr = (fn p => fn f => f (Deconstructor.deconstr p))
       in
          fn reconstructor => fn output => fn state =>
               fn (nt,prod) => pdeconstr prod (mk_deconstructor reconstructor output state nt)
       end

val abs_syntax = parser grammar
val abs_semantics = semantics abs_syntax
val abs_stripped = strip_grammar abs_semantics

(* hack for printing id-closures
val idc = idcl abs_semantics
val lcsp =  List.foldl (fn (x,r) => r^(if r="" then "" else " ")^x) ("")
val idcc = fn nt => 
     List.foldl
       (fn ((x,y),r) =>
           (r^(if r="" then "[" else "\n | [")^((lcsp x)^"]={"^(lcsp y)^"}")))
    "" (idc nt)
*)

val deconstructor = unify abs_syntax abs_semantics

val debug_reconstructor = false

fun reconstructor deconstructor output state =
    fn (input as (nt,p)) =>
      let fun corecur (input as (nt,p)) =
              if not debug_reconstructor 
                 then deconstructor output state input
                 else
                    let fun info dir = "reconstructor: "^dir^
                                      "corecur "^nt^" "^(prodToString p)^"\n"
                    in print (info ">");
                       deconstructor output state input
                       before (print (info "<"))
                    end 
         fun corecur_input _ = corecur input
         val out = (output state) o Induction.Repr.rep_str
         
      in Induction.Repr.prt_repr
             corecur_input
             corecur_input
             corecur_input
             corecur_input
             corecur_input
             out
             corecur_input
            p
      end

in
   val cache = cache
   val printer : (repr -> repr -> repr) -> repr -> string * repr -> repr
         = Induction.induction deconstructor reconstructor 
   val grammar = grammar
   val start = start
   local val terminalProd = Induction.Repr.rep_str o Induction.Str.fromMLString
         val sem_parser = grammar_to_parser terminals terminalProd Constructor.constr cache
   in
      val parser = fn s => (cache := Empty;sem_parser abs_stripped (NT(start,"")) (toinput s))
      val parser_ = fn i => (cache := Empty;sem_parser abs_stripped (NT(start,"")) i)
   end
end
end
