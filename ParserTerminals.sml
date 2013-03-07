structure ParserTerminals:> ParserTerminals =
struct

open Parser
open ParserMonad

infixr **>
infix |||
infix >>

fun inrange s1 s2 c =
   let val c1 = String.compare (s1, c)
   in
      (c1 = LESS orelse c1 = EQUAL) andalso
       let val c2 = String.compare (c, s2)
       in c2 = LESS orelse c2 = EQUAL
       end
   end

fun isa s c = 
   String.compare (s, c) = EQUAL

fun isn'ta s c = 
    not (String.compare (s, c) = EQUAL)

fun inlist [] _ = false
  | inlist (h::t) c = isa h c orelse inlist t c

val is_ws = inlist [" ", "\t", "\n"]

val parse_ws = 
      (parse_one is_ws **> (parse_while "ws" is_ws))
         >> (fn (s1,s2) => (Substring.span(s1,s2)))

val parse_optws = 
      (((parse_one is_ws **> (parse_while "ws" is_ws))
           >> (fn (s1,s2) => (Substring.span(s1,s2))))
   ||| (always >> (fn _ => (Substring.all ""))))

val parse_AZs = 
   let val pred = inrange "A" "Z"
   in
      (parse_one pred **> (parse_while "AZs" pred))
         >> (fn (s1,s2) => (Substring.span(s1,s2)))
   end

val parse_azs = 
   let val pred = inrange "a" "z"
   in
      (parse_one pred **> (parse_while "azs" pred))
         >> (fn (s1,s2) => (Substring.span(s1,s2)))
   end

val parse_azAZs = 
   let val pred = 
        fn c => inrange "A" "Z" c
         orelse inrange "a" "z" c
   in
      (parse_one pred **> (parse_while "azAZs" pred))
         >> (fn (s1,s2) => (Substring.span(s1,s2)))
   end

val parse_notdquote = 
   let val pred = isn'ta "\""
   in
      parse_while "notdquote" pred
   end

val parse_notgt = 
   let val pred = isn'ta ">"
   in
      parse_while "notgt" pred
   end

val parse_notsquote = 
   let val pred = isn'ta "'"
   in
      parse_while "notsquote" pred
   end

val parse_lws_a = parse_a_ilws

end (* struct *)
