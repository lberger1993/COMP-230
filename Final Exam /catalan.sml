(************************
**** LUCIA BERGER*******
      260473661
*)



datatype exp = Apply of exp * exp | If of exp * exp * exp | Let of exp * (string * exp) | Fun of string * string * exp | Plus of exp * exp | Mult of exp * exp | And of exp * exp | Eq of exp * exp | Not of exp | Var of string | Nat of int | Bool of bool

type Binding = string

type Environment = Binding list

fun merge cmp ([], ys) = ys
  | merge cmp (xs, []) = xs
  | merge cmp (xs as x::xs', ys as y::ys') =
      case cmp (x, y) of true => y :: merge cmp (xs, ys')
                       | _       => x :: merge cmp (xs', ys);

fun merge_sort cmp [] = []
  | merge_sort cmp [x] = [x]
  | merge_sort cmp xs = let
      val ys = List.take (xs, length xs div 2)
      val zs = List.drop (xs, length xs div 2)
    in
      merge cmp (merge_sort cmp ys, merge_sort cmp zs)
    end;

fun uniq_from_sorted [] = []
  | uniq_from_sorted [x] = [x]
  | uniq_from_sorted (x::y::xs) = if x = y then uniq_from_sorted (x::xs) else x :: uniq_from_sorted (y::xs)

fun contains x [] = false
  | contains x (y::ys) = if x = y then true else contains x ys;

 fun parse env vars (Bool _) = vars
   | parse env vars (Nat _) = vars
   | parse env vars (Var s) =
       if contains s env  then vars
         else s::vars
   | parse env vars (Not e) = parse env vars e
   | parse env vars (Eq (e1, e2)) =
       let
         val vars' = parse env vars e1
       in
         parse env vars' e2
       end
   | parse env vars (And (e1, e2)) =
       let
         val vars' = parse env vars e1
       in
         parse env vars' e2
   end
   | parse env vars (Mult (e1, e2)) =
       let
         val vars' = parse env vars e1
       in
         parse env vars' e2
       end
   | parse env vars (Plus (e1, e2)) =
       let
         val vars' = parse env vars e1
       in
         parse env vars' e2
       end
   | parse env vars (Fun (name, v, e)) = parse (v::name::env) vars e
   | parse env vars (Let (e1, (v, e2))) =
       let
         val vars' = parse env vars e1
       in
         parse (v::env) vars' e2
       end
   | parse env vars (If (e1, e2, e3)) =
       let
         val vars' = parse env vars e1
         val vars'' = parse env vars' e2
       in
         parse env vars'' e2
       end
   | parse env vars (Apply (e1, e2)) =
       let
         val vars' = parse env vars e1
       in
         parse env vars' e2
       end;

fun free_list e =
    uniq_from_sorted(merge_sort (op <) (parse [] [] e));

val ex1 = Let(Nat(5),("a",(Plus(Var("a"),Nat(2)))));
val ex2 = Plus(Var("a"),Nat(2));
val ex3 = Let(Var("y"),("x",Plus(Var("x"),Var("z"))));
val ex4 = Let(Nat(3),("z",(Let(Nat(2),("y",ex3)))));
val ex5 = Fun("fac","n",
        If(Eq(Var("n"),Nat(0)),Nat(1),Mult(Var("n"),(Apply(Var("fac"),
     (Mult(Var("n"),Nat(1))))))));
val ex6 = Fun("f","n",Plus(Nat(3),Var("n")));
val ex7 = Fun("g","n",
        If(Eq(Var("n"),Nat(0)),Nat(0),
     Plus(Nat(1),Apply(Var("g"),(Plus(Var("n"),Nat(~1)))))));

(*
val mini_language_test_1 = null (free_list ex1);
val mini_language_test_2 = free_list ex2 = ["a"];
val mini_language_test_3 = free_list ex3 = ["y", "z"];
val mini_language_test_4 = null (free_list ex4);
val mini_language_test_5 = null (free_list ex5);
val mini_language_test_6 = null (free_list ex6);
val mini_language_test_7 = null (free_list ex7);*)



(* PROBEM 2*)
fun catalan(n) =
  let
    val lst = List.tabulate(n + 1, fn x => x)
    val f = fn(x, init) => if (x=0) then 1 else (4*(x-1)*init + 2*init) div ((x-1)+2)
  in
    foldl f 1 lst
  end



(*Catalan number using lazy streams *)

(*Part 2*)

(*Problem 2*)

datatype realSeq = Cons of real * (unit -> realSeq);

fun takeN(num:int, seq:realSeq) : real = 
  case (num,seq) of
    (0, Cons(h,t)) => h
    |(_, Cons(h,t)) => takeN(num-1, t());

(*when used with argument n=0, this stream can be used to compute Catalan numbers*)
fun helperSeq(n:int) : realSeq = Cons(((4.0 * real(n) + 2.0)/(real(n)+2.0)), fn() => helperSeq(n+1));

(*when given argument n=0 CatalanSeq(n) produces the infinite stream of Catalan numbers*)
fun catalanSeq(n) = 
  let 
    val localHelpSeq = helperSeq(0)
    fun catalanNum(a:int):real = 
      case a of
        0 => 1.0
      |_ => takeN(a-1, localHelpSeq) * catalanNum(a-1)
  in
    Cons(catalanNum(n), fn() => catalanSeq(n+1))
  end;

(* PROBLEM 3 - Memory cell*)

datatype 'a instr = Put of 'a | Get | Restore

exception NoData

fun makeCell n =
let
  val data = ref [n]
  fun event (Put n) = 
        let
          val l = !data
        in
          data := n::l; n
        end
    | event Get =
        let
          val l = !data
        in
          if null l
          then (print "Nothing to get" ; raise NoData) else hd l
        end
    | event Restore =
        let
          val l = !data
        in
          if null l
          then (print "Nothing to restore" ; raise NoData) else let val (x::xs) = l in (data := xs ; x) end
        end
in
  event 
end;

val cell0 = makeCell 0;
val test_cell_put5 = cell0 (Put(5));
val test_cell_put6 = cell0 (Put(6));
val test_cell_get = cell0 Get;
val test_cell_restore_first = cell0 Restore;
val test_cell_restore_second = cell0 Restore;
val test_cell_restore_third = cell0 Restore;
val test_cell_restore_fourth = cell0 Restore;

