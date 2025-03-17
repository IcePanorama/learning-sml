(* affine_cypher
 * See: https://exercism.org/tracks/sml/exercises/affine-cipher. *)

(* See: https://en.wikipedia.org/wiki/Euclidean_algorithm. *)
fun euclidGCF (a, b) =
  if a < b then
    euclidGCF (b, a)
  else if b = 0 then
    a
  else
    euclidGCF (b, a mod b)
;

fun throwKeyANotCoprimeErr a = 
      raise Fail ("keyA (" ^ (Int.toString a) ^ ") and 26 are not coprimes.")
;

fun validateKeyA (a) =
  let
    (* See: https://en.wikipedia.org/wiki/Coprime_integers. *)
    fun isCoprime (x, y) = (euclidGCF (x, y)) = 1
  in
    if not (isCoprime (a, 26)) then throwKeyANotCoprimeErr a else NONE
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

(* Returns distance from #"a". *)
fun getIdxOfChar c = (Char.ord c) - (Char.ord #"a");

(* Where `i` should be the offset from #"a". *)
fun getCharFromIdx i = Char.chr (i + Char.ord(#"a"));

(* See: https://en.wikipedia.org/wiki/Affine_cipher. *)
fun encryptStr ("", a, b)  = ""
  | encryptStr (str, a, b) =
    let
      val _ = validateKeyA a

      val strNoWhitespace  = removeWhitespace (String.explode str)
      val indices          = map getIdxOfChar strNoWhitespace
      val encryptedIndices = map (fn i => (((a * i) + b) mod 26)) indices
      val encryptedChars   = map getCharFromIdx encryptedIndices

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

(* See: https://en.wikipedia.org/wiki/Affine_cipher. *)
fun decryptStr ("", a, b)  = ""
  | decryptStr (str, a, b) =
    let
      val _ = validateKeyA a

      (* See:
       *  https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/.
       *)
      fun modularMultiplicativeInv (a, m) =
        if (euclidGCF (a, m)) > 1 then
          throwKeyANotCoprimeErr a
        else
          let
            fun calcMMI (x, a, m) =
              if (((a mod m) * (x mod m)) mod m) = 1 then
                x
              else
                calcMMI (x+1, a, m)
          in
            calcMMI (0, a, m)
          end

      val strNoWhitespace  = removeWhitespace (String.explode str)
      val indices          = map getIdxOfChar strNoWhitespace
      val invA             = modularMultiplicativeInv (a, 26)
      val lhsVals          = map (fn y => (invA * (y - b)) mod 26) indices
      val decryptedIndices = map getCharFromIdx lhsVals
    in
      String.implode decryptedIndices
    end
;

(*
val input = "the quick brown fox jumps over the lazy dog";
val keyA  = 19;
val keyB  = 13;
val encryptedOutput = encryptStr (input, keyA, keyB);
print (encryptedOutput ^ "\n");
print ((decryptStr (encryptedOutput, keyA, keyB)) ^ "\n");
*)

(* TODO: handle command-line input. *)
fun throwImproperUsageErr () =
  let
    val usage = "Usage:\n\
      \\t./affine_cipher [OPTIONS] -a [A KEY] -b [B KEY] [ACTION]\n\n\
      \Actions:\n\
      \\t-e \"[input]\" # for encryption\n\
      \\t-d \"[input]\" # for decryption"
  in
    raise (Fail ("Improper usage error.\n" ^ usage))
  end
;

fun processArgs nil = throwImproperUsageErr ()
  | processArgs (flag::arg::tail) =
    let
      fun unrecognizedFlagErr f =
        let
          val _ = print ("Unrecognized flag: " ^ f ^ "\n")
        in
          throwImproperUsageErr ()
        end
    in
      (case (String.map Char.toLower flag) of
        "-a" => NONE
      | "-b" => NONE
      | "-e" => NONE
      | "-d" => NONE
      | f    => (unrecognizedFlagErr f))
    end
  | processArgs _ = NONE
;

val args = CommandLine.arguments ();
val _ = processArgs args;
