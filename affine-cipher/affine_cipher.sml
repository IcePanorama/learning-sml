(* affine_cypher
 * See: https://exercism.org/tracks/sml/exercises/affine-cipher. *)

fun validateKeyA (a) =
  let
    (* See: https://en.wikipedia.org/wiki/Coprime_integers. *)
    fun isCoprime (x, y) =
      let
        (* See: https://en.wikipedia.org/wiki/Euclidean_algorithm *)
        fun euclidGCF (a, b) =
          if a < b then
            euclidGCF (b, a)
          else if b = 0 then
            a
          else
            euclidGCF (b, a mod b)
      in
        (euclidGCF (x, y)) = 1
      end
  in
    if not (isCoprime (a, 26)) then
      raise Fail ("keyA (" ^ (Int.toString a) ^ ") and 26 are not coprimes.")
    else
      NONE
end
;


(* Returns the given list of characters w/o any whitespace chars. *)
fun removeWhitespace nil       = nil
  | removeWhitespace [c]       = if Char.isSpace c then nil else [c]
  | removeWhitespace (c::tail) =
  let
    val rest = removeWhitespace tail
  in
    if Char.isSpace c then
      rest
    else
      [c] @ rest
  end
;

(* See: https://en.wikipedia.org/wiki/Affine_cipher. *)
fun encryptStr ("", a, b)  = ""
  | encryptStr (str, a, b) =
    let
      val _ = validateKeyA a

      (* Returns distance from #"a". *)
      fun getIdxOfChar c =
        if not (Char.isAlpha c) then
          raise Fail ((Char.toString c) ^ " is not a letter.")
        else
          (Char.ord c) - (Char.ord #"a")

      val strNoWhitespace  = removeWhitespace (String.explode str)
      val indices          = map getIdxOfChar (strNoWhitespace)
      val encryptedIndices = map (fn x => ((a * x + b) mod 26)) indices
      fun idxToChar i      = Char.chr (i + Char.ord(#"a"))
      val encryptedChars   = map idxToChar encryptedIndices

      fun outputBuilder (nil, idx)         = nil
        | outputBuilder (head::tail, idx)  =
          if (idx mod 5) = 0 andalso idx <> 0 then
            [#" ", head] @ (outputBuilder (tail, idx + 1))
          else
            [head] @ (outputBuilder (tail, idx + 1))
    in
      String.implode (outputBuilder (encryptedChars, 0))
    end
;

(*
fun decryptStr ("", a, b)  = ""
  | decryptStr (str, a, b) =
;
*)

val input = "the quick brown fox jumps over the lazy dog";
val keyA  = 19;
val keyB  = 13;
print ((encryptStr (input, keyA, keyB)) ^ "\n");
