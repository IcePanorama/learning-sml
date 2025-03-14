(* acronym - generates an acronym for a given string.
 * See: https://exercism.org/tracks/sml/exercises/acronym. *)
fun removeHyphens (str) =
  String.map (fn x => if x = #"-" then #" " else x) str
;

fun removePunctuation (str) =
  let
    val strList = String.explode str
    val output = []
    fun buildOutput (ch, acc) =
        if Char.isAlpha (ch) orelse ch = #" " then
          ch::acc
        else
          acc
  in
    String.implode (foldr buildOutput output strList)
  end
;

fun trimStr "" = ""
  | trimStr (s) =
    let
      val strList = String.explode s

      fun trimFront nil = nil
        | trimFront (f::tail) =
          if f = #" " then
            trimFront tail
          else
            f::tail

      fun trimEnd nil = nil
        | trimEnd L =
          let
            val revList = List.rev L
            val trimmedRevList = trimFront revList
          in
            List.rev trimmedRevList
          end
    in
      String.implode (trimEnd (trimFront strList))
    end
;

fun createAcronym "" = ""
  | createAcronym s =
    let
      fun tokenizeString "" = nil
        | tokenizeString str = String.tokens (fn ch => ch = #" ") str

      fun getFirstLetter str = Char.toUpper (hd (String.explode str))
    in
      String.implode (map getFirstLetter (tokenizeString s))
    end
;

fun processInput "" = print ("\n")
  | processInput i =
    let
      val noHyphens = removeHyphens i
      val noPunct = removePunctuation noHyphens
      val trimmed = trimStr noPunct
      val acro = createAcronym trimmed
    in
      print (acro ^ "\n")
    end
;

fun runProgram (args) =
    let
      val input = String.concatWith " " args
    in
      processInput input
    end
;

val args = CommandLine.arguments ();
val _ =
  if (List.length args) = 0 then
    let
      val _ = print "Enter a sentence: "
    in
      case (TextIO.inputLine TextIO.stdIn) of
        SOME input => processInput input
      | NONE => raise Fail "Improper usage error."
    end
  else
    runProgram args
;
