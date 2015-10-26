(* LUCIA BERGER | 260473661
Sources include Professor Brigitte Pientka mcgill notes, which reference linked-lists, 
and last years solutions to similar problems. Sources also include Stack Overflow & Wikipedia sml 
 *)




(* PROBLEM 1 *
***************
insert into a linkedlist 

That is, insert takes three arguments: A comparison function of type ('a * 'a -> bool),an element of type ‘a, and a linked list lst of type ('a list) ref. Your function should destructively update the list lst

*)

datatype 'a rlist = Empty | RCons of 'a * (('a rlist) ref);

fun insert(x,q,lst as ref Empty)  =
    (lst := RCons(q,ref Empty))
    | insert(x,q,lst as ref (RCons(v,rest))) =
           if    x(v,q)
           then  insert(x,q,rest)
           else  lst := RCons(q,ref (RCons(v,rest)));



(* PROBLEM 2 *
***************
make_account 
Modify this code to implement a password protected bank account.
Any transaction would only be possible if one provides the correct 
password.

****************
This function take two arguments, an opening balance and a password.
 It returns a function which, when given a transaction and the correct password,
will perform the transaction. If the
password is incorrect, the function will print wrong passcode message

 *)

datatype transactions =
         Withdraw of int | Deposit of int | Check_balance;


fun make_account(open_balance:int) =
    let
        val balance = ref open_balance
    in
	fn
	    (Withdraw(i)) => (balance := !balance - i; !balance)
           |(Deposit(i))  => (balance := !balance + i; !balance)
	   |Check_balance =>  !balance
    end;


fun make_protected_account(open_balance:int, password:string) =
    let
        val account   = make_account(open_balance)
    in
        fn (trans,pass) =>
            if   pass = password
            then (account)(trans)
            else (print ("Wrong password.\n"); 0)
    end;
