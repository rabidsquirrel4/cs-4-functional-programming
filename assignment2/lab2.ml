(* Author:          Rachel Shi 
   Assignment Due:  Jan 30, 2020 2 AM
   Recieved a extension until Jan 30, 2020 2 PM from Professor Vanier*)
   
open Num

(* A.1 *)

(* The space complexity of the tree-recursive fibonacci function is O(n). 
    This is different from the time complexity because ocaml always 
    evaluates the first argument and then the second argument before 
    evaluating the expression. Thus, by the time ocaml gets to evaluating 
    the second argument in the else case edition addition, another call to 
    fib, it is already done with the space used to evaluate the first 
    argument, and doesn't need to use that space anymore. Thus, the recursive
    program never gets to using more than O(n) space at any given time. *)

(* A.2.1 *)

(* Since (12.15 / (3.0 ^ 5)) < 0.1 and (12.15 / (3.0 ^ 4)) > 0.1,
    the function p is applies 5 times when sine 12.15 is evaluated. *)

(* A.2.2 *)

(* The growth in space when sine a is evaluated is O(log(a)) since if a is 
    multiplied by 3, the number of calls to p on the stack is only incremented
    by 1. Then, since the operations in each call to p and sine are relatively 
    constant, the growth in the number of steps is logarithmic compared to 
    the growth of a. 
    The growth in the number of steps is also O(log(a)) since the number of 
    steps is also directly proportional to the number of times p is called. *)

(* A.3.a *)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with
      | 0 -> 1
      | n' when is_even n' -> square (fast_expt b (n / 2)) 
      | _ -> b * fast_expt b (n - 1)

(* A.3.b *)
let ifast_expt b n = 
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec ifast_expt_helper b n a = 
    match n with
      | 0 -> a
      | n' when is_even n' -> ifast_expt_helper (square b) (n / 2) a 
      | _ -> ifast_expt_helper b (n - 1) (b * a)
  in ifast_expt_helper b n 1

(* A.4 *)
let rec fast_mult a b = 
  let is_even m = m mod 2 = 0 in
  let double m = 2 * m in
  let halve m = m / 2 in  
    match b with 
    | 0 -> 0
    | b' when is_even b' -> fast_mult (double a) (halve b)
    | _ -> a + fast_mult a (b - 1)

(* A.5 *)
let ifast_mult b n = 
  let is_even m = m mod 2 = 0 in
  let double m = 2 * m in
  let halve m = m / 2 in
  let rec ifast_mult_helper b n a = 
    match n with
      | 0 -> a
      | n' when is_even n' -> ifast_mult_helper (double b) (halve n) a 
      | _ -> ifast_mult_helper b (n - 1) (b + a)
  in ifast_mult_helper b n 0

(* A.6 *)

(* The time complexity of foo is O(n), since the number of 
  recursive calls to foo forms a tree that is log(n) levels deep, and has
  n leaf nodes. Each call to foo is O(1) time complexity, and the number of 
  total calls to foo is then dominated by the leaves. Thus the time complexity
  of foo is O(n^(log_2(2))) = O(n). 
  The space complexity of foo is O(log(n)) because applicative-order 
  evaluation means that the first argument will always be fully evaluated
  before everything else. When the function is evaluated, the stack only goes 
  log(n) deep at each time since once it is done evaluating one side of the 
  tree, it goes up and no longer needs the space used form the previous branch
  it visited, allowing that space to be used for the next computation(s). *)

(* A.7.1 *)

(* This generates a linear recursive process because it is a linear process,
    and thus takes O(n) stack frames, but it must do computation
    after the recursive call, so it cannot be optimized through tail recursion. *)

(* A.7.2 *)

(* The space complexity is O(n) and the time complexity is also O(n). *)

(*** Part B. ***)

(* B.1.a *)

(* 
let x = 20
and y = 2 * 4
in x * (2 + y)

(fun x y -> x * (2 + y)) 20 (2 * 4)
*)


(* B.1.b *)

(* 
let a = 1.0
and b = 20.0
and c = 3.0
in sqrt (b *. b -. 4.0 *. a *. c)

(fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0
*)

(* B.1.c *)

(*
let x = 1 in
let y = 2 in
let z = 3 in
  x * y * z

(fun x -> 
let y = 2 in
let z = 3 in
  x * y * z
) 1

(fun x -> (fun y -> 
let z = 3 in
  x * y * z) 2) 1

(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1

*)

(* B.1.d *)

(*
let x = 1 in
let x = 2 in
let x = 3 in
  x * x * x

(fun x -> 
let x = 2 in
let x = 3 in
  x * x * x) 1

(fun x -> (fun x ->
(let x = 3 in
  x * x * x)) 2) 1

(fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1
*)

(* B.2 *)

(* 
let x = 2 * 10
and y = 3 + 4
in
  let y = 14 in
  let z = 22 in
    x * y * z

-Desugar this to:
(fun x y -> 
  let y = 14 in
  let z = 22 in
    x * y * z) (2 * 10) (3 + 4)

-Desugar this to:
(fun x y -> 
  (fun y ->
  let z = 22 in
    x * y * z) 14) (2 * 10) (3 + 4)

-Desugar this to:
(fun x y -> 
  (fun y ->
    (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)

-Evaluate: (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
  -Evaluate: (2 * 10) -> 20
  -Evaluate: (3 + 4) -> 7
  -Evaluate: ((fun y -> (fun z -> x * y * z) 22) 14) to: 
    ((fun y -> (fun z -> x * y * z) 22) 14)
  -Apply (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) to 20, 7.
    -Substitute 20 for x and 7 for y in the fun expression,
      (fun x y -> (fun y -> x * y * 22) 14), to get: 
        -Lambda sheilding occurs on y, since it is a variable in a nested 
            fun expression:
        -> (fun y -> (fun z -> 20 * y * z) 22) 14
      -Result: (fun y -> (fun z -> 20 * y * z) 22) 14
  -Evaluate (fun y -> (fun z -> 20 * y * z) 22) 14
    -Evaluate 14 -> 14
    -Evaluate (fun y -> (fun z -> 20 * y * z) 22) to 
      (fun y -> (fun z -> 20 * y * z) 22)
    -Apply (fun y -> (fun z -> 20 * y * z) 22) to 14.
      -Substitute 14 for y in the fun expression to get:
        (fun z -> 20 * 14 * z) 22
    - Result: (fun z -> 20 * 14 * z) 22
  -Evaluate (fun z -> 20 * 14 * z) 22
    -Evaluate 22 -> 22
    -Evaluate (fun z -> 20 * 14 * z) to (fun z -> 20 * 14 * z)
    -Apply (fun z -> 20 * 14 * z) to 22
      -Substitute 22 for z in the fun expression to get:
        20 * 14 * 22
    -Result: 20 * 14 * 22
  -Evaluate 20 * 14 * 22 -> 280 * 22
  -Evaluate 280 * 22 -> 6160
-Result: 6160
*)

(* B.3 *)

(* 
Desugar:

let x = 10
and y = x * 2
and z = y + 3
in x + y + z

(fun x y z -> x + y + z) 10 (x * 2) (y + 3)

This doesn't work because when the "and" keyword is used with expressions,
the expressions are evaluated separately before hand before being 
inserted into function as fully evaluated arguments. Thus, x = 10 is not in scope 
when y is evaluated, and y is not in scope when is evaluated. 

A simple way to fix this would be to use successive let expressions like so:
let x = 10 in
  let y = x * 2 in
    let z = y + 3 in x + y + z

*)

(*** Part C. ***)

let ni = num_of_int     (* convert int -> num *)

(* C.1 *)
(* let rec sum term a next b =
  if a >/ b
     then (ni 0)
     else term a +/ (sum term (next a) next b) *)

let isum term a next b =
  let rec iter a result =
    if a >/ b
       then result 
       else iter (next a) (term a +/ result)
  in
    iter a (ni 0)

(* C.2.1 *)
let product_iter term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (term a */ result)
  in
    iter a (ni 1)

(* C.2.2 *)
let rec product_rec term a next b =
  if a >/ b
     then (ni 1)
     else term a */ (product_rec term (next a) next b)

let factorial_iter n = product_iter (fun x -> x) (ni 1) (fun x -> x +/ (ni 1)) n
let factorial_rec n = product_rec (fun x -> x) (ni 1) (fun x -> x +/ (ni 1)) n
let pi_product n = (ni 4) */ (product_iter (fun x -> x */ (x +/ (ni 2))) 
                    (ni 2) (fun x -> x +/ (ni 2)) ((ni 2) */ n)) 
                    // (product_iter (fun x -> x */ x) (ni 3) 
                    (fun x -> x +/ (ni 2)) ((ni 2) */ n +/ (ni 1)))
let pi_approx = float_of_num (pi_product (ni 4000))

(* C.3.1 *)
let accumulate_iter combiner null_value term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (combiner (term a) result)
  in
    iter a null_value

(* C.3.2 *)
let rec accumulate_rec combiner null_value term a next b =
  if a >/ b
     then null_value
     else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

let sum = accumulate_iter (+/) (ni 0)
let product = accumulate_iter ( */ ) (ni 1) 

(* C.4 *)

let compose f g = (fun x -> f (g x))

(* C.5 *)

let repeated f n =
  let identity x = x in 
  let rec helper f n result =
    match n with 
      | 0 -> identity
      | 1 -> result
      | _ -> helper f (n-1) (compose f result)
  in
    helper f n f
(* C.6 *)

let smooth dx f x =
  (f (x -. dx) +. f(x) +. f (x +. dx)) /. 3.0

(* repeated smooth n *)

let nsmoothed dx f n x = 
  (repeated (smooth dx) n) f x

(* D.1 *)
let is_prime n =
  let rec check_factor n x =
    if int_of_float (sqrt (float_of_int n)) < x
      then true
    else 
      match x with 
        | x' when n mod x' = 0 -> false 
        | 2 -> check_factor n (x + 1)
        | _ -> check_factor n (x + 2)
  in
  match n with
    | n' when n' <= 1 -> false
    | _ -> check_factor n 2

(* D.2 *)
let smallest_prime_factor n =
  let rec find_factor n x =
    match x with 
      | x' when (n mod x' = 0 && is_prime x') -> x'
      | 2 -> find_factor n (x + 1)
      | _ -> find_factor n (x + 2)
  in
  match n with
    | n' when n' < 2 -> invalid_arg "input is less than 2"
    | n' when is_prime n'-> invalid_arg "input is prime"
    | n' -> find_factor n 2
