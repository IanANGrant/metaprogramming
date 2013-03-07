functor MapJoinSemantics 
   (val module_name : string
    datatype maparg = Copy of int | Update of int
    type repr
    val map_name : string
    val map : ((string * string) * string * int list * maparg list) list
    structure Induction : Induction
       where type repr = repr
    structure Deconstructor : Deconstructor
       where type repr = Induction.repr
    structure Constructor : Constructor
       where type repr = Induction.repr
   ) :> Semantics where type repr = repr =
struct
   local
      fun lookup m t c =
          let fun iter [] = NONE
                | iter ((v as ((t',c'),_,_,_))::vs) = 
                    if t = t' andalso c = c' then SOME v else iter vs
          in iter m
          end
   in
      type state = ((string * string * repr list -> repr) -> 
                     string * string * repr list -> repr) option
      type repr = repr
      val initstate = NONE
      val map_name = map_name
      fun deconstr (state:state) repr =
          let val (cmd as (t,c,l)) = (Deconstructor.deconstr repr)
                  handle Fail s => 
                       raise Fail (module_name^".deconstr: excn Fail "^s^" ("
                                     ^(Induction.reprToString repr)^")")
              fun errexcn n = Fail (module_name^".deconstr: subscript "^(Int.toString n)^
                              " out of range in ("^t^"."^c^" "^(Induction.reprListToString l)^")")
              fun argref v n = 
                     if n < 1 orelse n > List.length v
                        then raise (errexcn n)
                        else List.nth (v,n-1)
              fun gen_state (t,c,l) (t',c',l') =
                 fn constr => fn (t'',c'',l'') =>
                    let fun arglist args =
                           let fun iter r [] = rev r
                                 | iter r (p::ps) =
                                    let val a =
                                        case p of
                                           Copy n => argref l n
                                         | Update n => argref l'' n
                                    in iter (a::r) ps
                                    end
                           in iter [] args
                           end
                    in constr (t,c,arglist l')
                    end
          in
              case lookup map t c of
                  SOME (_,c',mapargs,joinargs) =>
                         ((map_name,c',List.map (fn n => argref l n) mapargs),
                          SOME (gen_state cmd (map_name,c',joinargs)))
                | NONE => (cmd,state)
          end

      fun reconstr state (cmd as (t,c,l)) = 
         let fun excn s = Fail (module_name^".reconstr: Fail ("^s^" "^t^"."^c^
                                  " "^(Induction.reprListToString l)^")")
         in case state of
              SOME gen_state =>
               if t = map_name then
                 (case (c,l) of
                     ("id",[e]) => (e,state)
                   | (_,_) => ((gen_state Constructor.constr cmd,state)
                                                 handle Fail s => raise (excn s)))
               else (Constructor.constr cmd,state)
            | NONE => (Constructor.constr cmd,state)
         end
   end
end
