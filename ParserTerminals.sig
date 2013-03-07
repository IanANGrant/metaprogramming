signature ParserTerminals =
sig
  val inrange : string -> string -> string -> bool
  val isa : string -> string -> bool
  val isn'ta : string -> string -> bool
  val inlist : string list -> string -> bool
  val is_ws : string -> bool
  val parse_ws : Substring.substring Parser.parser
  val parse_optws : Substring.substring Parser.parser
  val parse_AZs : Substring.substring Parser.parser
  val parse_azs : Substring.substring Parser.parser
  val parse_azAZs : Substring.substring Parser.parser
  val parse_notdquote : Substring.substring Parser.parser
  val parse_notsquote : Substring.substring Parser.parser
  val parse_notgt : Substring.substring Parser.parser
  val parse_lws_a : string -> Substring.substring Parser.parser
end
