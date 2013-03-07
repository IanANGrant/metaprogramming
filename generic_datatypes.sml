Meta.orthodox();

structure Poly =
struct
   datatype 'a comb = Var of string
                    | Comb of 'a * 'a
   val e1 = Var "x"
   val e2 = Comb(Var "y",Var "z")
   val e3 = Comb(e1,e2)
end

structure Recursive =
struct
   datatype comb = Var of string
                 | Comb of comb * comb
   val e1 = Var "x"
   val e2 = Comb(Var "y",Var "z")
   val e3 = Comb(e1,e2)
end

(* fails unifying 'a and 'a comb
fun poly_to_rec (Poly.Var s) = Recursive.Var s
  | poly_to_rec (Poly.Comb (e1,e2)) = Recursive.Comb (poly_to_rec e1,poly_to_rec e2)
*)

signature B =
sig
   type 'a X
   val f : 'a X -> Recursive.comb
   val g : Recursive.comb -> 'a X
end

structure O : B =
struct
   type 'a X = 'a Poly.comb
   fun f (Poly.Var s) = Recursive.Var s
     | f _ = raise Fail "O.f: no case"
   fun g (Recursive.Var s) = Poly.Var s
     | g _ = raise Fail "O.g: no case"
end

functor S(P : B) :> B
   where type 'a X = ('a P.X) Poly.comb =
struct
   type 'a X = ('a P.X) Poly.comb
   val f = fn Poly.Comb(e,e') => Recursive.Comb(P.f e,P.f e')
            | Poly.Var s => Recursive.Var s
   val g = fn Recursive.Comb(e,e') => Poly.Comb(P.g e,P.g e')
            | Recursive.Var s => Poly.Var s
end

structure One = S(O)
structure Two = S(One)
structure Five = S(S(S(Two)))

open Recursive
open Poly
val t = Five.f Poly.e3
val I = One.f o One.g
val t' = "OK" before ignore (I Recursive.e3)
               handle Fail s => "Exception: Fail: "^s


fun pinj (Poly.Comb(Poly.Var "x",Poly.Comb(Poly.Var "y",Poly.Var "z"))) =
         (Recursive.Comb(Recursive.Var "x",Recursive.Comb(Recursive.Var "y",Recursive.Var "z")))
  | pinj _ = raise Fail "no case"

fun pinj' (Recursive.Comb(Recursive.Var "x",Recursive.Comb(Recursive.Var "y",Recursive.Var "z"))) =
          (Poly.Comb(Poly.Var "x",Poly.Comb(Poly.Var "y",Poly.Var "z")))
         
  | pinj' _ = raise Fail "no case"

val id1 = pinj o pinj'
val id2 = pinj' o pinj

val t = pinj Poly.e3

structure Rectypes =
struct
   datatype str = String of string

   datatype atype = Tyvar of str
                  | Forall of str * atype
                  | Function of atype * atype

   datatype pat = Bindvar of str * atype

   datatype exp = Var of str
                | Bindexp of bind
                | Atypeexp of atype
                | Comb of exp * exp
   and bind = Lambda of str * exp | Abs of pat * exp

   datatype reprrepr = Nil' | Repr of repr * repr
   and repr = 
         Repexp of exp | Repbind of bind | Reppat of pat
       | Repatype of atype | Repstr of str | Reprepr of reprrepr

   val value = 
       Repexp(Bindexp(Lambda(String "Y",
                             Bindexp(Abs(Bindvar(String "x",
                                                 Forall(String "X",
                                                        Function(Tyvar(String "V"),
                                                                 Tyvar(String "Y")))),
                                         Comb(Comb(Var (String "x"),
                                                   Atypeexp(Tyvar(String "U"))),
                                              Var(String "v")))))))
end

structure Empty =
struct
   datatype 'a mu = Mu of 'a mu -> 'a
end

structure Polytypes =
struct
   datatype ('u,'v,'w,'x) exp = Var of 'u
                              | Bindexp of 'v
                              | Atypeexp of 'w
                              | Comb of 'x * 'x

   datatype ('u,'w) pat = Bindvar of 'u * 'w
   datatype ('u,'v,'w) bind = Lambda of 'u * 'w | Abs of 'v * 'w

   datatype ('u,'v) atype = Tyvar of 'u
                          | Forall of 'u * 'v
                          | Function of 'v * 'v

   datatype 'u str = String of 'u

   datatype 'u reprrepr = Nil' | Repr of 'u * 'u

   datatype ('r,'s,'t,'u,'v,'w) repr = 
         Repexp of 'r | Repbind of 's | Reppat of 't
       | Repatype of 'u | Repstr of 'v | Reprepr of 'w

   fun repr x y1 y2 y3 y4 y5 y6 = x y1 y2 y3 y4 y5 y6

   fun mk_var s = Var s
   fun mk_bindexp b = Bindexp b
   fun mk_atypeexp t = Atypeexp t
   fun mk_comb e e' = Comb (e,e')

   fun mk_lambda s e = Lambda (s,e)
   fun mk_abs p e = Abs (p,e)

   fun mk_bindvar s t = Bindvar (s,t)

   fun mk_tyvar s = Tyvar s
   fun mk_forall s t = Forall (s,t)
   fun mk_function t t' = Function(t,t')
   fun mk_string s = String s
   fun mk_nil' () = Nil'
   fun mk_reprrepr r r' = Repr (r,r')

   fun dst_exp var bindexp atypeexp comb =
            fn Var s => var s
             | Bindexp b => bindexp b
             | Atypeexp t => atypeexp t
             | Comb (e,e') => comb e e'

   fun dst_bind lambda abs =
            fn Lambda (s,e) => lambda s e
             | Abs (v,e) => abs v e

   fun dst_pat bindvar =
            fn Bindvar (s,t) => bindvar s t

   fun dst_atype tyvar forall function =
            fn Tyvar s => tyvar s
             | Forall (v,t) => forall v t
             | Function (t,t') => function t t'

   fun dst_str string = fn String s => string s

   fun dst_reprrepr nil' reprrepr =
            fn Nil' => nil' ()
             | Repr (r,r') => reprrepr r r'

   fun dst_repr exp bind pat atype str repr =
          fn Repexp e => exp e
           | Repbind e => bind e
           | Reppat e => pat e
           | Repatype e => atype e
           | Repstr e => str e
           | Reprepr e => repr e

   val mk_exprep = Repexp
   val mk_bindrep = Repbind
   val mk_patrep = Reppat
   val mk_atyperep = Repatype
   val mk_strrep = Repstr
   val mk_reprrep = Reprepr

   fun h x mk_exprep mk_bindrep mk_patrep mk_atyperep mk_strrep mk_reprrep =
           dst_repr (mk_exprep   o (dst_exp mk_var mk_bindexp mk_atypeexp mk_comb))
                    (mk_bindrep  o (dst_bind mk_lambda mk_abs))
                    (mk_patrep   o (dst_pat mk_bindvar))
                    (mk_atyperep o (dst_atype mk_tyvar mk_forall mk_function))
                    (mk_strrep   o (dst_str mk_string))
                    (mk_reprrep  o (dst_reprrepr mk_nil' mk_reprrepr)) x

(* e.g. ΛY.λx:ΠX.V→Y.x U v *)
   val value = 
          Repexp(Bindexp(Lambda(String "Y",
                                Bindexp(Abs(Bindvar(String "x",
                                                    Forall(String "X",
                                                           Function(Tyvar(String "V"),
                                                                    Tyvar(String "Y")))),
                                            Comb(Comb(Var (String "x"),
                                                      Atypeexp(Tyvar(String "U"))),
                                                 Var(String "v")))))))

   val value'' = 
          Bindexp(Lambda(String "Y",
                                Bindexp(Abs(Bindvar(String "x",
                                                    Forall(String "X",
                                                           Function(Tyvar(String "V"),
                                                                    Tyvar(String "Y")))),
                                            Comb(Comb(Var (String "x"),
                                                      Atypeexp(Tyvar(String "U"))),
                                                 Var(String "v"))))))

   val value' = h value mk_exprep mk_bindrep mk_patrep mk_atyperep mk_strrep mk_reprrep

end

structure PolyN =
struct
   datatype str = String of string

   datatype ('a,'b) name = Genus of 'a
                         | Species of 'a * 'b

   datatype ('a) nomial = Name of 'a

   datatype ('a,'b) meta = Cons of 'b * 'a

   val t = 
      Cons(Name(Species(String "w",
                  Genus(String "m"))),
               (Cons(Name(Species(String "w",
                            Genus(String "m"))),
                     Name(Genus(String "g")))))
end

structure RecN =
struct
   datatype str = String of string
   datatype name = Genus of str
                 | Species of str * name
   datatype meta = Name of name
                 | Cons of meta * meta
   val t = 
      Cons(Name(Species(String "w",
                  Genus(String "m"))),
               (Cons(Name(Species(String "w",
                            Genus(String "m"))),
                     Name(Genus(String "g")))))
end
