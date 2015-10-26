(* 

LUCIA EVE BERGER 
STUDENT ID: 260473661 

*)

exception Found of (int * string) list;

datatype 'a tree = Empty
		 | Node of (int * 'a) * 'a tree * 'a tree

						     
(* --------------
PROBLEM 1.1
----------------*)

 fun collect p Empty = []
 	| collect p t =
		let 
	(* propagate (int*string) list with exceptions via collect1*)
			fun collectHelp p Empty = raise Found []
			| collectHelp p (Node ((x,y),l,r)) =
				collectHelp p l handle Found left => 
				collectHelp p r handle Found right => 
					if (p (x)) (* check the int key of the node *)
					then 
						raise Found (left@[(x,y)]@right)
					else
						raise Found (left@right)
		in 
			collectHelp p t handle Found answer => answer (* last handle, handles results exception *)
		end;

						     
 
				
(*--------------
PROBLEM 1.2
-----------------
 gather :  (int -> bool)  -> string tree -> ((int * string) list -> 'a) : 'a *)

fun gather p Empty cont = []
  | gather p (Node ((x, y), left, right)) cont =
      if p x
      then (gather p left (fn _ => (x, y) :: gather p right cont))
      else (gather p left (fn _ => gather p right cont));



(*----------------
 PROBLEM 3.1
-------------------*)

type 'a church = ('a -> 'a) * 'a -> 'a
fun subCreate 0 (f,x) = x
    | subCreate n (f,x) = f (subCreate (n-1) (f,x))
fun create n = (fn (f,x) => subCreate n (f,x));

(*----------------
 PROBLEM 3.2
-------------------*)

fun churchToInt (cn) = cn ((fn x => x + 1), 0);

(*----------------
 PROBLEM 3.3: return its successor
-------------------*)

val succ =
  fn c => fn (f, x) => f(c (f,x));
