(* two_fer - Creates a sentence of the form 'One for X, one for me.'
 * See: https://exercism.org/tracks/sml/exercises/two-fer. *)
fun getFirstArg nil = ""
  | getFirstArg [a] = a
  | getFirstArg (a::_) = a
;

fun buildYouStr "" = "One for you"
  | buildYouStr name = "One for " ^ name
;

fun twoFer name =
  let
    val youStr = buildYouStr name
  in
    print (youStr ^ ", one for me.\n")
  end
;

val args = CommandLine.arguments ();
val name = getFirstArg args;
twoFer name;
