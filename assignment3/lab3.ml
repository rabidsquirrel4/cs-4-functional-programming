(* Author:          Rachel Shi 
   Assignment Due:  Feb 5, 2020 2 AM *)

(* A.1 *)   
let rec last_sublist = function
  | [] -> invalid_arg "last_sublist: empty list"
  | [t] -> [t]
  | h :: t -> last_sublist t

(* A.2 *)
let reverse lst = 
  let rec iter input output =
    match input with 
      | [] -> output
      | h :: t -> iter t (h :: output)
  in
    iter lst [] 

(* A.3 *)
let rec square_list = function
  | [] -> []
  | h :: t -> (h * h) :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(* A.4 *)

(* Defining square list in this way results in the answer list 
  being in reverse order because the items at the head of the 
  orginial list that appear first get added to the list 
  answer first, resulting in them being further into the list as
  new items are added on. 
  
  Interchanging the arguments in the constructor does not work either 
  because you cannot append an item to the end of a list of items using 
  the constructor. The constructor can only be used to "append" items to 
  the front of a list, since it is like an onion. *)


(* 
let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer @ [(h * h)])
  in iter items []

  I have modified Louis' second solution above to use the @ append
  function and formated (h*h) as part of a one item list. The 
  resulting function is not efficient because the @ operator can 
  take O(n) steps to add h*h to list where n is the length of 
  answer. *)

(* A.5 *)

let count_negative_numbers lst = 
  let rec iter input count = 
    match input with
    | [] -> count
    | h :: t when h < 0 -> iter t (count + 1)
    | _ :: t -> iter t count
  in 
    iter lst 0

(* A.6 *)
let power_of_two_list n = 
  let rec pow a b =
    match b with 
      | 0 -> 1
      | neg when neg < 0 -> invalid_arg "negative power"
      | _ -> a * (pow a (b - 1))
  in 
  let rec iter n result =
    match n with 
      | 0 -> result
      | neg when neg < 0 -> invalid_arg "negative argument for n"
      | n' -> iter (n' - 1) ((pow 2 (n' - 1)) :: result)
  in 
  iter n []

(* A.7 *)
let prefix_sum lst = 
  let rec inner sum input =
    match input with 
      | [] -> []
      | h :: t -> let new_sum = sum + h in 
                  new_sum :: inner new_sum t 
  in
  inner 0 lst

(* A.8 *)

let deep_reverse lst =
  let rec iter input output =
    match input with
      | [] -> output
      | h :: t -> iter t (reverse h :: output)
  in
    iter lst [] 

(* A.9 *)
type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list

let deep_reverse_nested lst =
  let rec iter input output =
    match input with
      | [] -> output
      | (Value v :: t) -> iter t (Value v :: output)
      | (List l :: t) -> iter t (List(iter l []) :: output)
  in
  match lst with
    | Value v -> Value v
    | List l -> List(iter l [])

(* B.1 *)

let rec filter predicate sequence =
  match sequence with
    | [] -> []
    | h :: t when predicate h -> h :: filter predicate t
    | _ :: t -> filter predicate t

let rec quicksort lst cmp = 
  match lst with
  | [] -> []
  | [t] -> [t]
  | pivot :: t -> quicksort (filter (fun x -> not (cmp pivot x)) t) cmp 
                  @ [pivot]
                  @ quicksort (filter (cmp pivot) t) cmp
(* B.2 *)

(* The quicksort function is an instance of generative recursion and 
  not structural recursion because we are generating new lists during the 
  quicksort process to recurse on. *)

(* B.3 *)

(* This merge sort won't work because if the list is non-empty, the function
    continues to infinitely call itself recursively on lists of size 1. This
    is because a list of size 1 in Ben's version is handled by the 
    general case that then calls merge sort on another list of size 1, causing
    an infinite loop. *)

(* B.4 *)

let rec insert_in_order new_result a_list cmp =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(* This is structural recursion because we are using the tail of the list
    we had before and not generating a new list to recurse on during each step. *)

(* C.1 *)

let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun lst -> h :: lst) rest)

(* This funciton works by setting one element in the list to the side and 
  then recursively calling itself to generate subsets of the rest of the 
  list. If we already have the subsets of the rest of the list, we know 
  that the additional subsets that include the element we set aside can simply 
  be generated by adding the element to the subsets we found earlier, since
  none of them contain the element we set aside but they are an exhaustive
  set of subsets of the rest of the list. *)

(* C.2 *)

let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
  accumulate (fun x r -> 1 + r) 0 sequence

(* C.3 *)

let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init (List.map (fun lst -> (List.hd lst)) seqs) 
                :: accumulate_n op init (List.map (fun lst -> (List.tl lst)) seqs)

(* C.4 *)

let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (xh :: xt, yh :: yt) -> f xh yh :: map2 f xt yt

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun row -> dot_product v row) m

let transpose mat = accumulate_n (fun x r -> x :: r) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
     map (matrix_times_vector cols) m