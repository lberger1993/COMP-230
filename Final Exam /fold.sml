
fun foldl(f, acc, lst) =
  case lst of
	[] => acc
| h::t => foldl(f, f(h,acc), t);

val y = map (hd, [[1,2],[3,4],[5,6,7]]);

let val x = 3
     val f = fn y => y + 3
     val x = 5
in
f 10
end