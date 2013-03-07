structure ParserMonad:> ParserMonad =
struct

infixr **>
infix |||
infix >>

(* The following are Tom Ridge's definitions of the parsers exactly as
   given his paper. Each seems to correspond quite closely with one of
   the three equations satisfied by any Kleisli triple, which in turn
   are due to the elementary properies of categories as structures
   having left and right inverses, and satisfying the associativity of
   composition. *)

(* The bind operator takes a pair of parsers to a parser of pairs, and
   it uses a map of a map to do this. The outer map applies the
   function f elementwise to the first parser, and the function f
   applies the anonymous inner function.

   The corrresponding monad law is Associativity: this is a 'double
   bind' operation, and the corresponding law for Kleisli triples is

        bind m (bind (λa.k a) λb.h b) = 
                bind (bind m λa.k a) λb.h b *)

(* ('a parser,'b parser) -> ('a * 'b) parser *)
fun (p1:('a Parser.parser)) **> (p2:('b Parser.parser)) = 
  fn (i:Parser.input) =>
    let fun f (e1,s1) =
       List.map (fn (e2,s2) => ((e1,e2),s2))
                (p2 {lc=(#lc i),sb=s1})
    in (List.concat o ((List.map f) o p1)) i
    end

(* Right Unit: bind m unit = m *)

(* ('a parser * 'a parser) -> 'a parser - A bit like a sum, but both
   could produce values at the same time. *)
fun (p1:'a Parser.parser) ||| (p2:'a Parser.parser) =
   fn (i:Parser.input) => (p1 i) @ (p2 i)

(* Left Unit: bind (unit a) k = k a *)

(* Simple composition with the function, taking an 'a parser via the
   map 'a -> 'b to a 'b parser. *)

(* 'a parser -> ('a -> 'b) -> 'b parser *)
fun (p:('a Parser.parser)) >> f =
   (List.map (fn (e,s) => (f e,s))) o p

(* Like the epsilon singleton {ε} in a regular algebra *)
val always:'a list Parser.parser = fn i => [([],Parser.substr i)]

(* Like the empty set ⌀ in a regular algebra *)
val never:'a list Parser.parser = fn i:Parser.input => []

end (* struct *)
