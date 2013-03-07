signature Syntax =
sig
   type repr
   eqtype str

   structure ExpParser : ProtoParser
      where type repr = repr

   structure TeXPrinter : ReprPrinter
     where type repr = repr

   structure UTF8Printer : ReprPrinter
     where type repr = repr

   val repr_parser : string -> repr
   val repr_parser_ : repr Parser.parser
end

functor Syntax
    (type repr
     eqtype str
     val grammar : string
     structure Induction : Induction
     where type repr = repr
       and type str = str
     structure Deconstructor : Deconstructor
        where type repr = Induction.repr
     structure Constructor : Constructor
        where type repr = Induction.repr
) :> Syntax
       where type repr = repr
       and type str = str =
struct
   open Induction 
   type repr = repr
   type str = str

   structure ExpParser =
       ProtoParser
           (type repr = repr
            val grammar = TextFile.read grammar
            val start = "TOP"
            val terminals = TerminalParsers.parser
            val cache:repr Parser.cachet = ref Parser.Empty
            structure Induction : Induction  
                where type repr = repr = Induction
            structure Deconstructor : Deconstructor
                where type repr = repr = Deconstructor 
            structure Constructor : Constructor
                where type repr = repr = Constructor 
           ) :> ProtoParser
               where type repr = repr

   val printer = ExpParser.printer

   structure UTF8 = 
      UTF8String
         (type repr = repr
          type str = str
          structure Induction : Induction = Induction
         ) :> Constructor
                 where type repr = repr

   structure TeX = 
      TeXString
         (type repr = repr
          type str = str
          structure Induction : Induction = Induction
         ) :> Constructor
                 where type repr = repr

   structure UTF8Printer =
       ReprPrinter
           (type repr = repr
            val printer = printer
            structure Induction : Induction = Induction
            structure Deconstructor : Deconstructor = Deconstructor
            structure Constructor : Constructor = UTF8
           ) :> ReprPrinter
               where type repr = repr 

   structure TeXPrinter =
       ReprPrinter
           (type repr = repr
            val printer = printer
            structure Induction : Induction = Induction
            structure Deconstructor : Deconstructor = Deconstructor
            structure Constructor : Constructor = TeX
           ) :> ReprPrinter
               where type repr = repr

   fun repr_parser s =
      let val rs = ExpParser.parser (s^";")
      in case rs
             of [(r,s)] => r
              | _ => raise Fail ("Syntax error")
      end
   val repr_parser_ = ExpParser.parser_
end
