(*********************************************************************
  leap_year - Given a year, prints whether that year is a leap year.
  See: https://exercism.org/tracks/sml/exercises/leap
 *********************************************************************)
fun isLeapYear (y : int) : bool =
  if (y mod 100) = 0 then
    (y mod 400) = 0
  else
    (y mod 4) = 0
;

fun argToInt (a : string) : int =
  case Int.fromString a of
    SOME n => n
  | NONE => raise (Fail "Improper usage error.")
;

fun processArgs nil = raise (Fail "Improper usage error.")
  | processArgs [x] = argToInt x
  | processArgs _   = raise (Fail "Improper usage error.")
;

val year = processArgs (CommandLine.arguments ());
val _ =
  let
    val res = isLeapYear year
  in
    if res then
      print ((Int.toString year) ^ " is a leap year.\n")
    else
      print ((Int.toString year) ^ " is not a leap year.\n")
  end
;
