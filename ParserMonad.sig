signature ParserMonad =
sig
  val **> : 'a Parser.parser * 'b Parser.parser -> ('a * 'b) Parser.parser
  val ||| : 'a Parser.parser * 'a Parser.parser -> 'a Parser.parser
  val >> : 'a Parser.parser * ('a -> 'b) -> 'b Parser.parser
  val always : 'a list Parser.parser
  val never : 'a list Parser.parser
end
