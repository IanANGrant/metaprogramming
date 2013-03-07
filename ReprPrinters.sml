functor OutputStream
   (type repr
    eqtype str
    structure Induction : Induction
       where type repr = repr
         and type str = str
    structure Constructor : Constructor
       where type repr = repr
   ) :> Constructor
       where type repr = repr =
struct
   type repr = repr
   type str = str
   local 
      val repr_list = Induction.reprListToString
   in
      fun constr (cmd as (t,c,l)) =
        case cmd of
           ("output","init",[]) =>
                  Constructor.constr ("output","nil",[])
         | ("output","out",[state,string]) =>
                  Constructor.constr ("output","cons",
                     [Constructor.constr ("output","text",[string]),state])
         | ("output","block",[state,name,contents]) =>
                  Constructor.constr ("output","cons",
                     [Constructor.constr ("output","block",[name,contents]),state])
         | ("output","result",state) =>
                  Constructor.constr ("output","result",state)
         | _ => Constructor.constr cmd
   end
end

functor TeXOutputStream
   (type repr
    eqtype str
    structure Induction : Induction
       where type repr = repr
         and type str = str
    structure Constructor : Constructor
       where type repr = repr
   ) :> Constructor
       where type repr = repr =
struct
   type repr = repr
   type str = str
   local
      fun assq k [] = k
        | assq k ((k',v)::ps) = if k = k' then v else assq k ps
      val trans = [("→", "\\rightarrow "), ("〈〉", "\\langle\\rangle "),
                   ("≡", "\\equiv "), ("←", "\\leftarrow "),("|","\\mid "),(" ","\\,"),
                   ("Π", "\\Pi "), ("·", "\\cdot "), ("〈", "\\langle "), ("〉", "\\rangle "),
                   ("λ", "\\lambdaup "), ("Λ", "\\Lambda "), ("μ", "\\mu "), ("∀","\\forall ")]
      val tytr =  [("α", "\\alpha "), ("β", "\\beta "), ("γ", "\\gamma "),
                   ("δ", "\\delta "), ("ε", "\\epsilon "), ("ζ", "\\zeta "),
                   ("η", "\\eta "), ("θ", "\\theta "), ("ι", "\\iota "),
                   ("κ", "\\kappa "), ("λ", "\\lambda "), ("μ", "\\mu "), ("ν", "\\nu "),
                   ("ξ", "\\xi "), ("φ", "\\varphi "), ("π", "\\pi "),
                   ("ϖ", "\\omegapi "), ("ρ", "\\rho "), ("ς", "\\varsigma "),
                   ("σ", "\\sigma "), ("τ", "\\tau "), ("υ", "\\upsilon "),
                   ("ϕ", "\\phi "), ("χ", "\\chi "), ("ψ", "\\psi "), ("ω", "\\omega ")]
      val repr_list = Induction.reprListToString
      val fromString = Induction.Repr.rep_str o Induction.Str.fromMLString
      val str = Induction.Str.toMLString o 
                  (Induction.dec_repr_str Induction.Repr.prt_repr)
   in
      fun constr (cmd as (t,c,l:repr list)) =
        let val constr = Constructor.constr
        in case cmd of
              ("output","text",[s]) =>
                   constr (t,c,[fromString(assq (str s) (trans@tytr))])
            | ("output","block",[b,s]) =>
                   constr ("output","text",[fromString("\\"^(str b)^"{"^(str s)^"}")])
            | _ => constr cmd
        end
   end
end

functor StringOutput
   (type repr
    eqtype str
    structure Induction : Induction
       where type repr = repr
         and type str = str
   ) :> Constructor
       where type repr = repr =
struct
   type repr = repr
   type str = str
   local
      val str = Induction.Str.toMLString o 
                  (Induction.dec_repr_str Induction.Repr.prt_repr)
      val fromString = Induction.Repr.rep_str o Induction.Str.fromMLString
      fun concat s1 s2 =
         let val s1' = str s1
             val s2' = str s2
         in Induction.Repr.rep_str (Induction.Str.fromMLString (s1'^s2'))
         end
      val repr_list = Induction.reprListToString
   in
      fun constr (cmd as (t,c,l:repr list)) =
        case cmd of
           ("output","nil",[]) => fromString ""
         | ("output","text",[s]) => s
         | ("output","block",[b,s]) => s
         | ("output","cons",[s1,s2]) => concat s2 s1
         | ("output","result",[state]) => state
         | _ => raise Fail ("StringOutput.constr: no case "^t^" "^c^" ["^(repr_list l)^"]")
   end
end

functor UTF8String(type repr 
                   eqtype str
                   structure Induction : Induction
                      where type repr = repr
                      and type str = str
   ) :> Constructor
       where type repr = repr =
   OutputStream(type repr = repr
                type str = str
                structure Induction : Induction = Induction
                structure Constructor : Constructor =
   StringOutput(type repr = repr
                type str = str
                structure Induction : Induction = Induction))

 functor TeXString(type repr 
                   eqtype str
                   structure Induction : Induction
                      where type repr = repr
                      and type str = str
   ) :> Constructor
          where type repr = repr =
   OutputStream(type repr = repr
                type str = str
                structure Induction : Induction = Induction
                structure Constructor : Constructor =
   TeXOutputStream(type repr = repr
                   type str = str
                   structure Induction : Induction = Induction
                   structure Constructor : Constructor =
   StringOutput(type repr = repr
                type str = str
                structure Induction : Induction = Induction)))
