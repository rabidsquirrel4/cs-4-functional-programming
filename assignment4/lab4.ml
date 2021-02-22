(* Author:          Rachel Shi 
   Assignment Due:  Feb 12, 2020 2 AM *)

(* A.1 *)
type point = {x: float; y: float}
type segment = {startp: point; endp: point}

let make_point x y = {x; y}

let get_coords {x; y} = (x, y)

let make_segment startp endp = {startp; endp}
let get_points {startp; endp} = (startp, endp)

let midpoint_segment {startp; endp} = 
  {x = (startp.x +. endp.x) /. 2.0; y = (startp.y +. endp.y) /. 2.0}

let segment_length {startp; endp} = 
  let xchange = startp.x -. endp.x 
  and ychange = startp.y -. endp.y in
  sqrt (xchange *. xchange +. ychange *. ychange)
let print_point {x; y} = Printf.printf "(%g, %g)\n" x y

(* A.2 *)

type rectangle = {ll: point; ur: point}

let rectangle_lower_segment {ll; ur} = 
  {startp=ll; endp={x=ur.x; y=ll.y}}
let rectangle_upper_segment {ll; ur} = 
  {startp={x=ll.x; y=ur.y}; endp=ur}
let rectangle_left_segment {ll; ur} =
  {startp=ll; endp={x=ll.x; y=ur.y}}
let rectangle_right_segment {ll; ur} =
  {startp={x=ur.x; y=ll.y}; endp=ur}

type rectangle2 = {lx: float; ly: float; ux: float; uy: float}
let rectangle_lower_segment2 {lx; ly; ux; uy} = 
  {startp={x=lx; y=ly}; endp={x=ux; y=ly}}
let rectangle_upper_segment2 {lx; ly; ux; uy} = 
  {startp={x=lx; y=uy}; endp={x=ux; y=uy}}
let rectangle_left_segment2 {lx; ly; ux; uy} = 
  {startp={x=lx; y=ly}; endp={x=lx; y=uy}}
let rectangle_right_segment2 {lx; ly; ux; uy} = 
  {startp={x=ux; y=ly}; endp={x=ux; y=uy}}

let rectangle_perimeter rect = (segment_length (rectangle_lower_segment rect)) 
  +. (segment_length (rectangle_upper_segment rect))
  +. (segment_length (rectangle_left_segment rect))
  +. (segment_length (rectangle_right_segment rect))
let rectangle_perimeter2 rect = (segment_length (rectangle_lower_segment2 rect)) 
  +. (segment_length (rectangle_upper_segment2 rect))
  +. (segment_length (rectangle_left_segment2 rect))
  +. (segment_length (rectangle_right_segment2 rect))
let rectangle_area rect = (segment_length (rectangle_lower_segment rect)) 
  *. (segment_length (rectangle_left_segment rect))
let rectangle_area2 rect = (segment_length (rectangle_lower_segment2 rect)) 
  *. (segment_length (rectangle_left_segment2 rect))
let make_rectangle ll ur = {ll; ur}
let make_rectangle2 lx ly ux uy = {lx; ly; ux; uy}

(* A.3 *)
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(* 
first (make_pair x y) 
evaluate make_pair x y -> (fun m -> m x y)
evaluate first -> (fun z -> z fun x y -> x) 
apply (fun z -> z fun x y -> x) to (fun m -> m x y):
  substitute:
  (fun m -> m x y) (fun x y -> x)
apply (fun m -> m x y) to (fun x y -> x):
  substitute:
  (fun x y -> x) x y 
apply (fun x y -> x) to any objects x, y:
  substitute: 
    x
yeilds x for any objects x and y
*)

(* Subsitution Model Evaluation of "second (make_pair 1 2)":
Evaluate (second (make_pair 1 2))
  -Evaluate (make_pair 1 2)
    -Evaluate 1 -> 1
    -Evaluate 2 -> 2
    -Evaluate make_pair x y -> fun m -> m x y
      -Desugar make_pair:
        (fun x y -> (fun m -> m x y))
    -Apply (fun x y -> (fun m -> m x y)) to 1, 2
    -Substitute 1 for x and 2 for y in the fun expression:
      -> (fun m -> m 1 2)
    -Result: (fun m -> m 1 2)
  -Evaluate second z -> z (fun x y -> y)
    -Desugar second: 
      (fun z -> z (fun x y -> y))
  -Apply (fun z -> z (fun x y -> y)) to (fun m -> m 1 2)
  -Substitute (fun m -> m 1 2) for z in the fun expression:
    -> (fun m -> m 1 2) (fun x y -> y)
    -Evaluate (fun m -> m 1 2) (fun x y -> y)
      -Evaluate (fun x y -> y) -> (fun x y -> y)
      -Evaluate (fun m -> m 1 2) -> (fun m -> m 1 2)
      -Apply (fun m -> m 1 2) to (fun x y -> y)
      -Substitute (fun x y -> y) for m in the fun expression:
        (fun x y -> y) 1 2
        -Evaluate (fun x y -> y) 1 2
          -Evaluate 1 -> 1
          -Evaluate 2 -> 2
          -Evaluate (fun x y -> y) -> (fun x y -> y)
          -Apply (fun x y -> y) to 1, 2
          -Substitute 1 for x and 2 for y in the fun expression:
            fun 1 2 -> 2
            2
          -Result: 2
  -Result: 2
*)

(* A.4 *)
let rec pow a b =
  match b with 
    | 0 -> 1
    | _ -> a * (pow a (b - 1))
let rec int_log a b = 
  match b with 
    | 0 -> 0
    | b' when b' mod a = 0 -> 1 + (int_log a (b / a))
    | _ -> 0
let make_pairi a b = (pow 2 a) * (pow 3 b)
let firsti x = int_log 2 x
let secondi x = int_log 3 x

(* A.5 *)
let zero = []
let is_zero = function
  | [] -> true
  | () :: _ -> false
let succ u = () :: u
let prev = function
  | [] -> invalid_arg "zero does not have a previous representation"
  | () :: t -> t
let rec integer_to_unary i =
  if i = 0
    then zero
  else 
    succ (integer_to_unary (i - 1)) 

let rec unary_to_integer u = 
  if is_zero u
    then 0 
  else
    1 + unary_to_integer (prev u)
let rec unary_add u1 u2 = 
  if is_zero u2 
    then u1
  else
    unary_add (succ u1) (prev u2) 
  
type nat = Zero | Succ of nat
let zero' = Zero
let is_zero' = function
| Zero -> true
| Succ _ -> false
let succ' u = Succ u
let prev' = function
  | Zero -> invalid_arg "zero does not have a previous representation"
  | Succ t -> t

(* No, the other definitions do not have to change from 
  their definitions in the previous representations,
  other than obvious name changes. *)
let rec integer_to_unary' i =
  if i = 0
    then zero'
  else 
    succ' (integer_to_unary' (i - 1)) 

let rec unary_to_integer' u = 
  if is_zero' u
    then 0 
  else
    1 + unary_to_integer' (prev' u)
let rec unary_add' u1 u2 = 
  if is_zero' u2 
    then u1
  else
    unary_add' (succ' u1) (prev' u2) 

(* A.6 *)

let zerof = fun s -> fun z -> z
let add1 n = fun s -> fun z -> s (n s z)
let one = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))
let add n m = fun s -> fun z -> (n s) (m s z)
let church_to_integer n = n (fun x -> x + 1) 0

(* A.7 *)

(* 
val church_to_integer : ((int -> int) -> int -> 'c) -> 'c
val zerof : 'a -> 'b -> 'b
"church_to_integer zerof" returns an integer because 'a in zerof
must be of type (int -> int) and 'b in zerof must be of type int
to match the expresion "((int -> int) -> int -> 'c)".
Thus, zerof in "church_to_integer zerof" must be of type 
((int -> int) -> int -> int) and following that, 'c is of type int.
Since c' is of type int, the expression "church_to_integer zerof" 
returns an integer.

val church_to_integer : ((int -> int) -> int -> 'c) -> 'c
val one : ('a -> 'b) -> 'a -> 'b
"church_to_integer one" returns an integer because both 'a
and 'b must be of type int to match the expression for the
argument to church_to_integer, ((int -> int) -> int -> 'c).
'c must also be of type int, since one is of type
(int -> int) -> int -> int. Thus, "church_to_integer one" 
must return an int, since it returns type 'c. *)

(*** Part B. ***)

(* B.1 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* B.1.1 *)

let left_branch (Mobile (l, r)) = l
let right_branch (Mobile (l, r)) = r
let branch_length = function
  | (Weight (l, w)) -> l
  | (Structure (l, m)) -> l
let branch_structure = function
  | (Weight (l, w)) -> `Weight w
  | (Structure (l, m)) -> `Structure m

(* B.1.2 *)

let rec branch_weight1 = function
  | (Weight (l, w)) -> w
  | (Structure (l, m)) -> total_weight1 m 
and total_weight1 (Mobile (l, r)) = branch_weight1 l + branch_weight1 r 

let rec branch_weight2 b = 
  match branch_structure b with
    | `Weight w -> w
    | `Structure m -> total_weight2 m
and total_weight2 m = branch_weight2 (left_branch m) 
                      + branch_weight2 (right_branch m) 

(* B.1.3 *)

let rec is_balanced m = 
  let l = (left_branch m)
  and r = (right_branch m) 
  in
  let b = (branch_weight2 l) * (branch_length l) = (branch_weight2 r) * (branch_length r)
  in
  (match branch_structure l with
    | `Weight w -> b
    | `Structure m' -> b && (is_balanced m'))
  &&
  (match branch_structure r with
    | `Weight w -> b
    | `Structure m' -> b && (is_balanced m'))

(* B.1.4 *)

type mobile' = { left : branch'; right : branch'; }
and branch' = Branch' of int * contents
and contents = Weight' of int | Structure' of mobile'
let make_mobile' left right = {left; right}
let make_weight' l w = Branch' (l, Weight' w)
let make_structure' l s = Branch' (l, Structure' s)
let left_branch' m = m.left
let right_branch' m = m.right
let branch_length' (Branch'(l, c)) = l
let branch_structure' (Branch'(l, c)) = 
  match c with 
    | Weight' c -> `Weight c
    | Structure' s -> `Structure s

let rec branch_weight' b = 
  match branch_structure' b with
    | `Weight w -> w
    | `Structure m -> total_weight' m 
and total_weight' m = branch_weight' (left_branch' m) 
+ branch_weight' (right_branch' m) 

let rec is_balanced' m =  
  let l = (left_branch' m)
  and r = (right_branch' m) 
  in
  let b = (branch_weight' l) * (branch_length' l) = (branch_weight' r) * (branch_length' r)
  in
  (match branch_structure' l with
    | `Weight w -> b
    | `Structure m' -> b && (is_balanced' m'))
  &&
  (match branch_structure' r with
    | `Weight w -> b
    | `Structure m' -> b && (is_balanced' m'))

(* B.2 *)

type tree = Tree of elem list
and elem = Num of int | Sub of tree
let rec square_tree (Tree(t)) = 
  let square_elem e =
    match e with
      | Num n -> Num (n * n)
      | Sub t -> Sub (square_tree t) 
  in
  let rec list_helper l =
    match l with 
      | [] -> []
      | h :: t -> square_elem h :: (list_helper t) 
  in 
  Tree (list_helper t)

let rec square_tree' (Tree(t)) = Tree (List.map 
  (function
    | Num n -> Num (n * n)
    | Sub t -> Sub (square_tree' t)) 
  t)

(* B.3 *)

let tree_map f (Tree(t)) = Tree (List.map 
(function 
  | Num n -> Num (f n)
  | Sub t -> Sub (square_tree' t)) 
t)

(*** Part C. ***)

(* C.1 *)

type expr =
    Int of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Pow of expr * int

let rec simplify1 = 
  let rec pow a b =
    match b with 
      | 0 -> 1
      | neg when neg < 0 -> invalid_arg "negative power"
      | _ -> a * (pow a (b - 1))
  in
  function
  | Add((Int i), (Int j)) -> Int(i + j)
  | Mul((Int i), (Int j)) -> Int(i * j)
  | Pow((Int i), j) -> Int(pow i j)
  | Add((Int 0), e) -> e
  | Add(e, (Int 0)) -> e
  | Mul(_, (Int 0)) -> Int(0)
  | Mul((Int 0), _) -> Int(0)
  | Mul(e, (Int 1)) -> e
  | Mul((Int 1), e) -> e
  | Pow(_, 0) -> Int(1)
  | Pow(e, 1) -> e
  | Add(e1, e2) -> Add(simplify1 e1, simplify1 e2)
  | Mul(e1, e2) -> Mul(simplify1 e1, simplify1 e2)
  | Pow(e, i) -> Pow(simplify1 e, i)
  | e -> e
let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e

(* C.2 *)

let rec deriv s = function
  | Int(i) -> Int(0)
  | Var(s') when s' = s -> Int(1)
  | Var(s') -> Int(0)
  | Add(e1, e2) -> Add(deriv s e1, deriv s e2)
  | Mul(e1, e2) -> Add(Mul(deriv s e1, e2), Mul(e1, deriv s e2))
  | Pow(e, i) -> Mul(Mul(Int(i), Pow(e, i - 1)), deriv s e) 
let derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d