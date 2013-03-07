structure TerminalParsers:> TerminalParsers =
struct
open Parser
open ParserMonad
open ParserTerminals

infixr **>
infix |||
infix >>

val subscripts = ["₀","₁","₂","₃","₄","₅","₆","₇","₈","₉"]

val parse_ptyvars = 
   let val lpred = 
        fn c => inrange "a" "z" c
         orelse inlist ["α", "β", "γ", "δ", "ε",
                        "ζ", "η", "θ", "ι", "κ", "ν",
                        "ξ", "ο", "π", "ϖ", "ρ", "ς",
                        "σ", "τ", "υ", "φ", "χ", "ψ", "ω"] c
        val rpred =
        fn c => lpred c orelse inlist ["λ","μ"] c
                        orelse inlist subscripts c
   in
      (parse_one_ilws lpred **>
       (parse_while "ptyvars" rpred))
         >> (fn (s1,s2) => (Substring.span(s1,s2)))
   end

val parse_atyvars = 
   let val lpred = 
        fn c => inrange "A" "Z" c
        val rpred =
        fn c => lpred c orelse inlist ["'","/","_"] c
                        orelse inrange "0" "9" c
   in
      (parse_one_ilws lpred **>
       (parse_while "atyvars" rpred))
         >> (fn (s1,s2) => (Substring.span(s1,s2)))
   end

val parse_vars = 
   let val lpred = 
        fn c => inrange "a" "z" c
         orelse inlist ["α", "β", "γ", "δ", "ε",
                        "ζ", "η", "θ", "ι", "κ", "ν",
                        "ξ", "ο", "π", "ϖ", "ρ", "ς",
                        "σ", "τ", "υ", "φ", "χ", "ψ", "ω"] c
        val rpred =
        fn c => lpred c orelse inlist ["λ","μ","_","'"] c
                        orelse inlist subscripts c
   in
      (parse_one_ilws lpred **>
       (parse_while "vars" rpred))
         >> (fn (s1,s2) =>
               (Substring.span(s1,s2)))
   end

val parse_tyconsts = 
   let val lpred = 
        fn c => inrange "A" "Z" c
        val rpred =
        fn c => lpred c orelse inrange "a" "z" c
                        orelse inlist subscripts c
   in
      (parse_one_ilws lpred **>
       (parse_while "tyconsts" rpred))
         >> (fn (s1,s2) =>
               (Substring.span(s1,s2)))
   end

val parse_lws_a = parse_a_ilws

val parse_ctyvar = 
   let val pred = 
        fn c => 
         inlist ["α", "β", "γ", "δ", "ε", "ζ",
                 "η", "θ", "ι", "κ", "λ", "μ", "ν",
                 "ξ", "ο", "π", "ϖ", "ρ", "ς", "σ",
                 "τ", "υ", "φ", "χ", "ψ", "ω"] c
   in
      parse_one_ilws pred
   end

fun assq p l =
   let fun iter [] = raise Fail ("Grammar error "^p)
         | iter ((k,v)::xs) =
               if (k=p) then v else iter xs
   in iter l end

fun parser tm =
   let val q = if String.size tm > 0 then String.substring (tm,0,1) else ""
       val stripquotes = fn tm =>
             String.substring (tm,1,(String.size tm) - 2)
   in 
      if q = "\"" then parse_a (stripquotes tm)
      else if q = "<" then parse_lws_a (stripquotes tm)
      else if q = "'" then parse_a (stripquotes tm)
      else assq tm [("?AZs?",parse_AZs),
                    ("?azAZs?",parse_azAZs),
                    ("?azs?",parse_azs),
                    ("?var?",parse_vars),
                    ("?tyconsts?",parse_tyconsts),
                    ("?ptyvar?",parse_ptyvars),
                    ("?atyvar?",parse_atyvars),
                    ("?notsquote?",parse_notsquote),
                    ("?notdquote?",parse_notdquote),
                    ("?notgt?",parse_notgt),
                    ("?optws?",parse_optws),
                    ("?ws?",parse_ws)]
   end
end
