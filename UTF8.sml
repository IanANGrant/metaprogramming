structure UTF8 :> UTF8 =
struct

exception BadUTF8 of string

val two11 = 2048  (* 2 ^ 11 *)
val two16 = 65536 (* 2 ^ 16 *)
val Umax = 0x10FFFF (* maximum Unicode code point *)

fun chr i =
    if i < 0 then raise Chr
    else if i < 128 then str (Char.chr i)
    else if i < two11 then let
        val w = Word.fromInt i
        val byte2 = 128 + Word.toInt (Word.andb(w, 0wx3F))
        val byte1 = 0xC0 + Word.toInt (Word.>>(w,0w6))
      in
        String.implode [Char.chr byte1, Char.chr byte2]
      end
    else if i < two16 then let
        val w = Word.fromInt i
        val byte3 = 128 + Word.toInt (Word.andb(w, 0wx3F))  (* 3F = 6 bits *)
        val w = Word.>>(w,0w6)
        val byte2 = 128 + Word.toInt (Word.andb(w, 0wx3F))  (* 3F = 6 bits *)
        val w = Word.>>(w,0w6)
        val byte1 = 0xE0 + Word.toInt (Word.andb(w, 0wxF))
           (* inital E says there are 3 bytes, and with F to extract 4 bits *)
      in
        String.implode (map Char.chr [byte1, byte2, byte3])
      end
    else if i <= Umax then let
        val w = Word.fromInt i
        val byte4 = 128 + Word.toInt (Word.andb(w, 0wx3F))  (* 3F = 6 bits *)
        val w = Word.>>(w,0w6)
        val byte3 = 128 + Word.toInt (Word.andb(w, 0wx3F))  (* 3F = 6 bits *)
        val w = Word.>>(w,0w6)
        val byte2 = 128 + Word.toInt (Word.andb(w, 0wx3F))  (* 3F = 6 bits *)
        val w = Word.>>(w,0w6)
        val byte1 = 0xF0 + Word.toInt (Word.andb(w, 0wx7))
           (* inital F says there are 4 bytes, and with 7 to extract 3 bits *)
      in
        String.implode (map Char.chr [byte1, byte2, byte3, byte4])
      end
    else raise Chr

fun byte1_count c = let
  fun recurse acc b = if b > 0w127 then recurse (acc + 1) (Word8.<<(b,0w1))
                      else acc
in
  recurse 0 (Word8.fromInt (Char.ord c))
end

fun isCont_char c = let val i = Char.ord c in 128 <= i andalso i < 192 end

fun pow2 i = Word.toInt (Word.<<(0w1, Word.fromInt i))

fun getFirstChar ss =
  let open Substring
      fun ucontinue acc pos limit ss =
      if pos = limit then let
          val (p,s) = splitAt (ss, limit)
        in
          ((p, acc), s)
        end
      else let
          val pos_c = sub(ss, pos)
              handle Subscript => raise BadUTF8 (string (slice(ss,0,SOME pos)))
        in
          if isCont_char pos_c then
            ucontinue (acc * 64 + Char.ord pos_c - 128) (pos + 1) limit ss
          else raise BadUTF8 (string (slice(ss,0,SOME (pos + 1))))
        end
   in
     case getc ss of
        NONE => NONE
      | SOME (c, ss') => let
          val i = Char.ord c
        in
          if i < 128 then SOME ((slice (ss,0,SOME 1),i), ss')
          else let
              val cnt = byte1_count c
            in
              if cnt = 1 then raise BadUTF8 (str c)
              else SOME (ucontinue (i + pow2 (8 - cnt) - 256) 1 cnt ss)
            end
        end
    end

fun getChars n s =
 let open Substring
  fun recurse r n' ss =
      if n>0 andalso n' = n 
      then (rev r,ss)
      else case getFirstChar ss of
                    SOME (p,ss') => recurse (p::r) (n'+1) ss'
                  | _ => (rev r,ss)
in
  recurse [] 0 (all s)
end

fun getChar s =
   let open Substring
   in
      case getFirstChar (all s) of
         SOME ((cs,i),ss') => SOME ((string cs,i), string ss')
       | NONE => NONE
   end

fun explode s =
   let open Substring
       val (l,ss') = getChars 0 s
   in
      List.map (fn (cs,i) => string cs) l
   end

fun index s =
   let open Substring
       val (l,ss') = getChars 0 s
   in
      List.map (fn (cs,_) => let val (_,i,_) = base cs in i end) l
   end

fun size s = let
  open Substring
  val ss = all s
  val sz = size ss
  fun recurse acc pos =
      if pos = sz then acc
      else let
          val c = sub(ss,pos)
        in
          if Char.ord c < 128 then recurse (acc + 1) (pos + 1)
          else let
              val bc = byte1_count c
            in
              check acc (pos + 1) pos (bc - 1)
            end
        end
  and check acc pos start cnt =
      if cnt = 0 then recurse (acc + 1) pos
      else if pos = sz then
        raise BadUTF8 (string (slice(ss,start,SOME(pos-start))))
      else let
          val c = sub(ss,pos)
        in
          if isCont_char c then check acc (pos + 1) start (cnt - 1)
          else raise BadUTF8 (string (slice(ss,start,SOME(pos-start))))
        end
in
  recurse 0 0
end

fun lastChar s = let
  open Substring
  val ss = all s
  val lastpos = size ss - 1
  fun goback pos =
      if pos < 0 then raise BadUTF8 (str (sub(ss,0)))
      else let
          val c = sub(ss,pos)
        in
          if Char.ord c >= 192 then let
              val bc = byte1_count c
            in
              if lastpos - pos + 1 = bc then string (slice(ss,pos,NONE))
              else raise BadUTF8 (string (slice(ss,pos+bc,NONE)))
            end
          else if isCont_char c then goback (pos - 1)
          else raise BadUTF8 (string (slice(ss,pos+1,NONE)))
        end
in
  if lastpos < 0 then NONE
  else let
      val c = sub(ss, lastpos)
    in
      if Char.ord c < 128 then SOME(str c, Char.ord c)
      else Option.map #1 (getChar (goback (lastpos - 1)))
    end
end

fun translate f s = let
  fun recurse i changed acc ustr =
      case getChar ustr of
        NONE => if changed then String.concat (List.rev acc)
                else s
      | SOME ((c,code), rest) => let
          val c' = f c
        in
          if c' = c andalso not changed then
            recurse (i + 1) changed acc rest
          else if not changed then
            recurse i true (c' :: String.extract(s,0,SOME i)::acc) rest
          else
            recurse i true (c' :: acc) rest
        end
in
  recurse 0 false [] s
end

end (* struct *)
