
(* abstract syntax ********************************************************)
(* the abstract syntax of arithmetic expressions *)

type var = string

datatype exp 
  = Nat of int              (* number constants *)
  | Bool of bool 
  | Mult of exp * exp
  | Var of var              (* variables, with name given by the var argument *)
  | Plus of exp * exp       (* addition: Plus(e1,e2) == e1 + e2 *)
  | Times of exp * exp      (* multiplication: Times(e1,e2) == e1 * e2 *)
  | Let of var * exp * exp  (* Let(x,e1,e2) == let x = e1 in e2 *)




(* varsets ********************************************************)
(* We want to compute sets of free variables, so we need to choose a 
 * data structure to represent such sets. We will represent sets of variables
 * as ordered lists of variables (i.e. strings). *)

(* REPRESENTATION INVARIANT:
 * a varset is a list of vars (strings) in ascending order, with
 * no duplicates *)

(* We introduce a type name for variable sets.  This is just an abbreviation
 * for "var list", which in turn is equivalent to "string list" *)

type varset = var list

(* the empty varset is represented by the empty list.  This satisfies the
 * representation invariant vacuously *)

val empty : varset = nil

fun insert (x: var, nil) = [x]
  | insert (x, (s as (y::ys))) =  
      if x < y then x :: s
      else if y < x then y :: insert(x,ys)
      else s


fun single (v: var) = insert(v, empty)


fun union (xs, nil) = xs
  | union (nil, ys) = ys
  | union (x::xs, ys) = insert (x, union(xs, ys))

(* the difference of two sets (often written a\b).  diff(a,b) consists
 * of all the elements of a that are not in b. *)
fun diff (xs: varset, nil) = xs
  | diff (nil, ys) = nil
  | diff (xl as (x::xs), yl as (y::ys)) = 
      if x < y then x :: diff (xs, yl)
      else if y < x then diff (xl, ys)
      else diff (xs, ys)


fun freeList (Exp) = empty
              | 

  | freeVars (Var v) = single v
  | freeVars (Plus(e1,e2)) = union (freeVars e1, freeVars e2)
  | freeVars (Times(e1,e2)) = union (freeVars e1, freeVars e2)
  | freeVars (Let(v,e1,e2)) = union (freeVars e1, (diff (freeVars e2, single v)))


(* test cases:
 *  try applying freeVars to these sample expressions *)

val exp1 = Let("x",Nat 3,Plus(Var "x", Nat 1))

val exp2 = Let("x",Var "y",Plus(Var "x", Nat 1))

val exp3 = Let("x",Nat 3,Plus(Var "x", Var "y"))

val exp4 = Let("x",Var "y",Plus(Var "x", Var "y"))

val exp5 = Let("x",Plus(Var "x",Nat 3),Plus(Var "x", Var "y"))

val exp6 = Let("x",Plus(Var "y",Nat 3),
               Let("y",Plus(Var "x", Nat 1),Times(Var "x", Var "y")))
