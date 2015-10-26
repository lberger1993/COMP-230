datatype complex = Complex of real * real
  (* Now we can't possibly confuse `complex' with `coord'.
     Downside: you have to write Complex(a,b) instead of (a,b),
     and pattern-match on Complex(a,b) in functions.
     I think this is worth it, though, to avoid nasty bugs.
     (A fairly similar error has caused some very expensive spacecraft to crash!)
  *)

(* Adding and multiplying with the `complex' datatype: *)
fun complexAdd (Complex(a,b), Complex(c,d)) =  Complex(a + c,    b + d)
                  (*    a+bi     +    c+di  =         (a + c) + (b + d)i  *)
                  fun complexMul (Complex(a,b), Complex(c,d)) =  Complex(a*c - b*d,   b*c + a*d)
                  (*    a+bi     +    c+di  =         (ac  - bd) + (bc  + ad)i *)