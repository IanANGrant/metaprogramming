signature SubsConvention =
sig
   type repr
   type state
   type exstate
   val map_name : string
   val initstate : state
   val get_state : state -> exstate
   val set_state : state -> exstate -> state
   val reconstr : state -> repr -> repr * state
   val consalpha : state -> repr -> repr -> state
   val consavoid : state -> repr -> state
   val nullavoid : state -> state
end
