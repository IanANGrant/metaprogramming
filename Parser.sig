signature Parser =
sig
  datatype 'a ml = Empty
                 | Link of 'a * 'a ml ref

  type 'a memo = (((int * (int * int)) list * (int * int)) * 
                       ('a * Substring.substring) list) ml ref

  type 'a cachet = (string * (int * 'a memo)) ml ref

  type context = (int * Substring.substring) list

  type input = {lc: context,sb: Substring.substring}
  type 'a parser = input -> ('a * Substring.substring) list

  val substr : input -> Substring.substring
  val toinput : string -> input

  val check_and_upd_lctxt : int -> 'a parser -> 'a parser
  val memop : 'a memo -> 'a parser -> 'a parser

  val parse_a :  string -> substring parser
  val parse_one : (string -> bool) -> substring parser
  val parse_while : string -> (string -> bool) -> substring parser
  val parse_a_ilws :  string -> substring parser
  val parse_one_ilws : (string -> bool) -> substring parser
  val parse_while_ilws : string -> (string -> bool) -> substring parser
  val content : substring -> string

end
