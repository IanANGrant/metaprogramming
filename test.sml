signature Sig =
sig
   type X
   datatype d = A of X
end

structure Struct :> Sig =
struct
   type X = int
   datatype d = A of X
end


structure Struct' :> Sig =
struct
   type X = string
   datatype d = A of X
end
