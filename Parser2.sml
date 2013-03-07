(* Ridge combinator parsing.

   Implemented directly from the two versions of the paper called
   "Simple, functional, sound and complete parsing for all
   context-free grammars." Dated 2010 and 2011, available from
   http://www.cs.le.ac.uk/~tr61.  This is a thing of beauty.

   We use memoisation as suggested in the 2010 paper, and it seems to
   work well. However, it is easy to write highly ambiguous
   gammars where the number of possible parses grows hyper-
   exponentially with the length of the input. For example the grammar
   "E -> E E | '1' | ''" parses the string "111" in six different
   ways, and the string "1111" in 344 different ways. See the
   AmbExample.sml file.  See the papers for an explanation of how the
   limit on recursion depth comes about. This is a very simple and
   _beautiful_ treatment of a tired old problem. When one sees it so
   clearly explained, one can't help wondering why one didn't think of
   it one's self.

   These parsers have also the very significant advantage of providing
   a natural recursive derivation of the production. This is because
   the parser cache and the context are a record of the search for a
   derivation, so they are the perfect basis for an explanation of
   exactly why the parser made the decisions it did. This record
   refers to the productions in the actual grammar, so it is
   intelligible and a useful form of explanation. So gone are the days
   of the error message "Syntax error at <EOF>" which gives the
   programmer no clue whatsoever about the cause of the error: all she
   learns from this message is that it is _not_ beyond the end of the
   source file! Using the context and cache we can answer any question
   she may have about what decision was made where, and do that in
   terms of the actual terminals and non-terminals as they appear in
   the formal specification of the language. This is most useful to
   her when she is designing the language itself. Detailed error
   diagnostics make debugging BNF _far_ easier.

   Nothing in this code is meant to be non-standard, and the substring
   implementation is from the SML Basis Library, so it should be easy
   enough to get it running under any other Standard ML interpreter
   such as PolyML, MLton or SMLNJ. The UTF8 module is a modified
   version of the one in HOL4. *)

structure Parser2 :> Parser2 =
struct

fun full s = Substring.all s

fun string ss =
   let val (s,_,_) = Substring.base ss
   in s end

fun subseq s1 s2 =
   let val (s,i,n) = Substring.base s1
       val (s',i',n') = Substring.base s2
   in i=i' andalso n=n' andalso s=s'
   end

fun subseqind s1 s2 =
   let val (s,i,n) = Substring.base s1
       val (s',i',n') = Substring.base s2
   in i=i' andalso n=n'
   end

fun len ss = Substring.size ss

fun low ss =
   let val (_,i,_) = Substring.base ss
   in i end

fun high ss =
   let val (_,i,n) = Substring.base ss
   in i+n end

fun stripss ss =
   let val (_,i,n) = Substring.base ss
   in (i,n) end

fun inc_low n' ss =
   Substring.triml n' ss

fun inc_high n' ss =
   let val (s,i,n) = Substring.base ss
   in Substring.substring(s,i,n+n') end

fun dec_high n' ss =
   Substring.trimr n' ss

type nonterm = int
type context = (nonterm * int) list

type input = {lc: context,sb: Substring.substring}

fun lift f = fn ({lc, sb}:input) => {lc=lc, sb=f sb}:input
fun substr ({lc, sb}:input) = sb
fun toinput s = {lc=[], sb=Substring.all s}:input

type 'a parser = input -> ('a * Substring.substring) list

(* The function addnt maintains the context in lexicographic order of
   the pairs (nt,n) where nt is the number of the non-terminal and n
   is the length of the input substring. *)

fun addnt ctxt (p' as (nt,n)) =
    let fun copyout r [] = rev r
          | copyout r (h::t) = copyout (h::r) t
        fun iter r [] = rev ((nt,n)::r)
          | iter r (l as ((p as (nt',n'))::ps)) =
               if nt < nt'
                  then copyout ((nt,n)::r) l
                  else (if nt = nt'
                        then (if n > n'
                              then iter (p::r) ps
                              else copyout ((nt,n)::r) l)
                        else iter (p::r) ps)
    in
       iter [] ctxt
    end

fun update_lctxt trim nt (p: 'a parser) =
   fn ({lc,sb}:input) => p {lc=addnt lc (nt,trim),sb=sb}

(* Context equivalence is used to decide whether a result in the cache
   applies to this call.  For this purpose, any two contexts c and c'
   with c=((nt_1,n_1),(nt_2,n_2),...) and
   c'=((nt'_1,n'_1),(nt'_2,n'_2),...) are equivalent, as far as a
   parser given an input length n'' is concerned, when the
   sub-sequences of the nonterminals nt_j and nt'_k such that
   n''=n_j=n'_k are permutations of each other. This is more precisely
   explained in Standard ML than it could be in `English'! *)

fun eqv_lctxts (l,n) =
   let fun empty [] = true
         | empty ((_,n')::t) =
              (not (n=n')) andalso empty t
       fun equiv [] [] = true
         | equiv [] (c2 as (_::t2)) = empty c2
         | equiv (c1 as (_::t1)) [] = empty c1
         | equiv (c1 as ((nt,n')::t1))
                 (c2 as ((nt',n'')::t2)) =
               if n=n'
               then if n=n''
                    then nt=nt' andalso equiv t1 t2
                    else equiv c1 t2
               else if n=n''
                    then equiv t1 c2
                    else equiv t1 t2
   in equiv
   end

(* All the reference manipulation is in the function memop *)

datatype 'a ml = Empty
            | Link of 'a * 'a ml ref

type 'a memo = (((int * int) list * (int * int)) * ('a * substring) list) ml ref

type 'a cachet = (string * (int * 'a memo)) ml ref

fun memop pad p:'a parser = 
   let fun mem t c s =
      let fun it (l as (ref Empty)) = 
                 let val r = t ()
                     val m = Link(((c,s),r),ref Empty)
                 in
                     l := m;
                     r
                 end
            | it (ref (Link(((c',s'),r),l))) =
                   if s=s' 
                      andalso eqv_lctxts s c c'
                   then r
                   else it l
      in it
      end
   in
      fn (i as ({lc,sb}:input)) =>
           mem (fn () => p i) lc (stripss sb) pad
   end

fun ignr_last (p:'a parser) = 
   fn i => 
     let val s = substr i
         val l = len s
     in if l = 0 
        then [] 
        else 
           let val s' = Substring.triml (l-1) s
               val dec = Substring.trimr 1
               fun inc (e,s) = (e,Substring.span (s,s'))
           in ((List.map inc) o p o (lift dec)) i
           end
     end

fun check_and_upd_lctxt nt (p:'a parser) =
   fn (i as ({lc,sb}:input)) =>
      let val n = len sb
          fun iter r [] = r
            | iter r ((nt',n')::ps) =
                let val r' = if nt'=nt andalso n'<r then n' else r
                in iter r' ps
                end
          val trim = iter n lc
      in
         if trim = 0 orelse len sb = 0
         then []
         else update_lctxt (trim - 1) nt p i
      end

fun prefix pred = fn ss =>
   let val (s,i,n) = Substring.base ss
   in if n = 0
      then Substring.splitAt (ss,0)
      else
         let fun f s l =
             if l = n
             then n
             else
               (let val (c,s') = 
                      case UTF8.getFirstChar s of
                           SOME ((cs,_),ss) =>
                              (Substring.string cs,ss)
                         | NONE =>
                              raise Fail "Empty UTF8 character?"
                in
                    if pred c 
                    then f s' (l + (String.size c))
                    else l
                end)  (* ignr_last truncates multi-byte chars, so: *)
                    handle UTF8.BadUTF8 _ => l
             val l = f ss 0
          in
             Substring.splitAt(ss, l)
          end
   end

val lws = prefix (fn s => List.exists (fn s' => s' = s) [" ","\t", "\n"])

fun parse_a_lws_maybe ilws lit =
   fn i =>
      let val ss = substr i 
          val (_,ss) = if ilws then lws ss else (Substring.splitAt (ss,0))
          val n = String.size lit
          val i = low ss
      in
         if (n <= len ss) andalso
                (String.substring(string ss,i,n) = lit)
         then [Substring.splitAt (ss,n)]
         else []
      end

val parse_a = parse_a_lws_maybe false
val parse_a_ilws = parse_a_lws_maybe true

fun parse_one_lws_maybe ilws pred =
   fn i =>
      (let val s = substr i
           val (_,s) = if ilws then lws s else (Substring.splitAt (s,0))
           val (c,s') = 
               case UTF8.getFirstChar s of
                  SOME ((c,_),s') => (Substring.string c,s')
                | NONE => ("",s)
       in
          if pred c 
          then [Substring.splitAt (s,String.size c)]
          else []
       end)  (* ignr_last truncates multi-byte chars, so: *)
       handle UTF8.BadUTF8 _ => []

val parse_one = parse_one_lws_maybe false
val parse_one_ilws = parse_one_lws_maybe true

fun parse_while_lws_maybe ilws msg pred =
   fn i =>
     let val ss = substr i
         val (_,ss) = if ilws then lws ss else (Substring.splitAt (ss,0))
         val (_,_,n) = Substring.base ss
      in if n = 0
         then [] 
         else [prefix pred ss]
   end

val parse_while = parse_while_lws_maybe false
val parse_while_ilws = parse_while_lws_maybe true

val content = Substring.string

end (* struct *)
