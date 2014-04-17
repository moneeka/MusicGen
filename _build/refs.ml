(* NAMES:
 *
 * Partner 1's name: ______
 * Partner 1's code.seas account: _______
 *
 * (Leave blank if you are working alone)
 * Partner 2's name: ______
 * Partner 2's code.seas account: _______
 *)

open Core.Std

(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)

let rec is_cyclic (a:'a mlist) (b:'a mlist) : bool =    
  match a with 
    | Nil -> false 
    | Cons (_, { contents = a }) ->
      match b with 
        | Nil | Cons (_, { contents = Nil }) -> false
        | Cons (_, { contents = Cons (_, {contents = b}) }) ->
          phys_equal a b || is_cyclic a b 
 
let has_cycle (lst : 'a mlist) : bool =
  is_cyclic lst lst

(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

(*>* Problem 1.2 *>*)
(* Write a function flatten that flattens a list (removes its cycles if it
 * has any) destructively. Again, you may want a recursive helper function and
 * you shouldn't worry about space. *)
  
 
let rec flatten_list (lst:'a mlist) (already_visited:'a mlist) : unit =
    match lst with
    |Nil -> ()
    |Cons(x, t) ->
        if phys_equal lst already_visited then t:= Nil
        else flatten_list (!t) (Cons(x, ref already_visited))

let flatten (lst : 'a mlist) : unit =
    if has_cycle lst then flatten_list lst lst
    else ()
    
(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

assert( not (has_cycle list1) );;
assert( has_cycle list2);;

let rec list1 = Cons(1, ref list1);;
assert(flatten list1; list1 = Cons(1, ref Nil));;
 

(*>* Problem 1.3 *>*)
(* Write mlength, which finds the number of nodes in a mutable list. *)
let rec count (lst : 'a mlist) (already_visited : 'a mlist) (counter : int) : int = 
        match lst with
        | Nil -> counter
        | Cons(x,t) -> count (!t) (Cons(x, ref already_visited)) (counter + 1)
        
let mlength (lst : 'a mlist) : int =
    if has_cycle lst then let cpy = lst in flatten cpy; 
    count cpy cpy 0 else
    count lst lst 0
 
let list_test = Cons(2, ref Nil)
let list_test2 = Cons(2, ref list_test)
let rec list1 = Cons(1, ref list1);;
assert(mlength list_test = 1);;
assert(mlength list_test2 = 2);;
assert(mlength list1 = 1);;
(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = -1 
