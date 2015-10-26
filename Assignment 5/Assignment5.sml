(************************
**** LUCIA BERGER*******
      260473661
*)


(* Problem 1 *)

type var = string

datatype exp = Nat of int | Bool of bool |Plus of exp * exp |
               Mult of exp * exp | If of exp * exp * exp |
               And of exp * exp | Not of exp | Eq of exp * exp |
               Var of string | Let of exp * (string * exp) |
               Fun of string * string * exp |
               Apply of exp * exp;


(* we want to compute the sets of free variables --> we've chosen string lists *)

(* REMOVE FUNCTION - auxiliary -> removes x from list l 





(* *)
fun remove (x, [])  = [] 
  | remove (x, l::ls) = 
      (*base case here *)
      if x = 1 
      then remove (x, ls)
      else l::remove (x,ls);

 
(* UNION - returns the union of two lists--> find the unio nbetween two lists --> find tahat         
*)
fun union (x:var, )



(**)

fun insert (x:var, nil) = [x]
	 | insert (x, (s as (y::ys))) =  
      if x < y then x :: s
      else if y < x then y :: insert(x,ys)
      else s *)


(*singlemton list from an element *)




fun member x l = List.exists (fn y => y = x )l
	

fun remove(x, [])   = []      (* auxiliary function removes element x from list l *)
 |  remove(x,l::ls) =
       if   x = l
       then remove(x,ls)
       else l::remove(x,ls);
UNION - returns the union of two lists (notice the return is a set, meaning it has dups removed)
fun union(a,b) =
  let  fun remDup ([])    = []                 (* auxiliary function removes all duplicates*)
        |  remDup (x::xs) = x::remDup(remove(x,xs))
  in remDup(a@b)
  end;




fun unionList sets = foldr(fn(s1,s2) => union(s1,s2)) [] sets 



*)


(*QUESTION 2 -- using foldl : catalan function*)


fun foldl(f, acc, lst) =
  case lst of
  [] => acc
  | h::t => foldl(f, f(h,acc), t);



fun Catalan (x) = 




fun Catalan 1 = 1
  | Catalan n = (fn k => (Calatan k) * (Catalan (n-k))) (n-1);

(*QUESTION 2: Part 2*)

datatype realSeq = Cons of real * (unit -> realSeq);

  (* set the val limit*) 
