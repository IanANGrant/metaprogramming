structure TextFile :> TextFile =
struct
   fun read f =
      let val istr = TextIO.openIn f
          fun iter r =  
              if TextIO.endOfStream istr
              then rev r 
              else let val s = TextIO.inputLine istr 
                    in iter (s::r) end
      in 
          String.concat (iter []) before
          TextIO.closeIn istr
      end
end
