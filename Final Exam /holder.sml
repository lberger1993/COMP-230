

(*Problm 3*)

exception emptyList;

datatype 'a instr = Put of 'a | Get | Restore;

fun makeCell(initialValue: int) = 
	let val cell = ref (initialValue::[])
	in
		fn(command) =>
			case command of
				Put(value) => 
				(case !cell of [] => (cell := (value::[]); value) 
					| (h::t) => (cell := (value::h::t); value))
			|	Get => 
				(case !cell of [] => raise emptyList(print "Nothing to get", 0) 
					| (h::t) => (cell := (h::t); h))
			|   Restore => 
				(case !cell of [] => raise emptyList(print "Nothing to restore", 0) 
					| (h::[]) => raise emptyList(print "Nothing to restore", 0) 
					| (a::b::t) => (cell := b::t; b))
	end;