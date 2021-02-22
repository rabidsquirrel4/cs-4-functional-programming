(* name: Rachel Shi *)
(* email: rshi@caltech.edu *)

(*** Part A. ***)

let replicate n v = 
  let rec helper n v return = 
  match n with 
    | n' when n' <= 0 -> return
    | n' -> helper (n' - 1) v (v :: return)
  in
  if n <= 0
    then []
  else helper n v []
let repln n lst =
  let rec helper n lst return = 
    match lst with 
    | [] -> return
    | h :: t -> helper n t (return @ (replicate n h))
  in
  if n <= 0
    then []
  else helper n lst []
let repln2 n lst = 
  let rec helper n lst return = 
    match n with 
    | n' when n' <= 0 -> return
    | n' -> helper (n - 1) lst (return @ lst)
  in
  if n <= 0
    then []
  else helper n lst []
let repln3 n lst = List.concat (replicate n lst)
let group lst = 
  let rec helper lst sublist = 
    match lst with 
      | [] -> sublist :: []
      | h :: t when sublist = [] -> helper t ([h])
      | h :: t when h = List.hd sublist -> helper t (h :: sublist)
      | h :: t -> sublist :: (helper t ([h]))
  in
  if lst = [] then [] else helper lst []
let rle lst = 
  let rec helper lst elem num =
  match lst with 
    | [] -> (elem, num) :: []
    | h :: t when h = elem -> helper t h (num + 1)
    | h :: t -> (elem, num) :: (helper t h 1)
  in
  match lst with 
    | [] -> [] 
    | h :: t -> helper t h 1
let rle2 lst = List.map (fun x -> (List.hd x, List.length x)) (group lst)
let rld lst = 
  let rec helper lst elem num = 
    match lst with
      | [] -> []
      | (e, n) :: t when num >= n -> helper t e 0
      | (e, n) :: t -> e :: (helper lst e (num + 1))
  in
  match lst with 
    | [] -> [] 
    | (e, n) :: t -> helper lst e 0
let make_grouper init update pred = fun lst ->
  let rec helper lst curr_elem = 
  match lst with 
    | [] -> curr_elem :: []
    | h :: t when pred h curr_elem -> helper t (update h curr_elem)
    | h :: t -> curr_elem :: (helper t (init h))
  in
  match lst with 
    | [] -> [] 
    | h :: t -> helper t (init h)
let group2 lst = 
  make_grouper
    (fun h -> [h])                    (* init function *)
    (fun h curr -> h :: curr)         (* update function *)
    (fun h curr -> h = List.hd curr)  (* pred function *)
    lst
let rle3 lst = 
  make_grouper
    (fun h -> (h, 1))                     (* init function *)
    (fun h curr -> (h, snd curr + 1))     (* update function *)
    (fun h curr -> h = fst curr)          (* pred function *)
    lst
let remove_nth n lst =
  let rec helper lst n removed result = 
    match lst with
      (* | [] when n >= 0 -> failwith "nth element not present" *)
      | [] -> (removed, List.rev result)
      | h :: t when n = 0 -> helper t (n-1) h result
      | h :: t -> helper t (n-1) removed (h :: result)
  in
  match n with 
    | n' when n' < 0 -> invalid_arg "remove_nth"
    | n' when n' >= List.length lst -> failwith "nth element not present"
    | n' -> helper lst n' (List.hd lst) []
let shuffle lst = 
  let rec helper lst = function
    | 0 -> lst
    | n -> let (elem, newlst) = remove_nth (Random.int n) lst in 
            (elem :: (helper newlst (n-1)))
  in
  helper lst (List.length lst) 

(* The asymptotic time complexity of shuffle is O(n^2) because each remove_nth
  call takes linearly less and less time and the first call takes O(n) time, 
  so the total complexity of the remove_nth calls is 
  O(n + (n-1) + ... + 1) = O((n * (n + 1))/2) = O(n^2 + n) = O(n^2). 
  Since the List.length computation is O(n), the O(n^2) time-complexity
  trumps the time complexity of the length computation, and thus the total
  time complexity is O(n). 
  *)
let rec gray_codes n = 
  match n with
  | n' when n' < 1 -> invalid_arg "gray_codes"
  | 1 -> [[0]; [1]]
  | n' -> let lst = gray_codes (n' - 1) in 
          (List.map (fun x -> 0 :: x) lst) @ 
          (List.map (fun x -> 1 :: x) (List.rev lst))

(*** Part B. ***)

type tree =
    Leaf
  | Node2 of tree * int * tree
  | Node3 of tree * int * tree * int * tree

let rec tree_search i t =
  match t with 
    | Leaf -> false 
    | Node2 (left, value, right) -> if value = i then true 
        else (tree_search i left) || (tree_search i right)
    | Node3 (left, valuel, middle, valuer, right) -> 
        if (valuel = i || valuer = i) then true
        else (tree_search i left) || (tree_search i middle) 
              || (tree_search i right)

type insertion = Ok of tree | Split of tree * int * tree
let rec insert_helper i t =
  match t with

    (* Base cases. *)

    | Leaf -> Ok (Node2 (Leaf, i, Leaf))

    | Node2 (_, j, _) when i = j -> Ok t  (* i is already in tree *)
    | Node3 (_, j, _, k, _) when i = j || i = k -> Ok t  (* ditto *)

    | Node2 (Leaf, j, Leaf) when i < j ->   (* add i to tree; change 2-node to 3-node *)
        Ok (Node3 (Leaf, i, Leaf, j, Leaf))
    | Node2 (Leaf, j, Leaf) ->   (* i > j *)
        Ok (Node3 (Leaf, j, Leaf, i, Leaf))

    | Node3 (Leaf, j, Leaf, k, Leaf) when i < j ->  (* split; watch the order! *)
        Split (Node2 (Leaf, i, Leaf), j, Node2 (Leaf, k, Leaf))
    | Node3 (Leaf, j, Leaf, k, Leaf) when i > j && i < k ->
        Split (Node2 (Leaf, j, Leaf), i, Node2 (Leaf, k, Leaf))
    | Node3 (Leaf, j, Leaf, k, Leaf) ->   (* i > k *)
        Split (Node2 (Leaf, j, Leaf), k, Node2 (Leaf, i, Leaf))

    (* Recursive cases. *)

    | Node2 (t1, j, t2) when i < j ->  (* insert into t1 *)
        begin
          match insert_helper i t1 with
            | Ok t1' -> Ok (Node2 (t1', j, t2))
            | Split (t1a, i', t1b) -> Ok (Node3 (t1a, i', t1b, j, t2))
        end

    | Node2 (t1, j, t2) ->  (* i > j; insert into t2 *)
        begin
          match insert_helper i t2 with
            | Ok t2' -> Ok (Node2 (t1, j, t2'))
            | Split (t2a, i', t2b) -> Ok (Node3 (t1, j, t2a, i', t2b))
        end

    | Node3 (t1, j, t2, k, t3) when i < j ->  (* insert into t1 *)
        begin
          match insert_helper i t1 with
            | Ok t1' -> Ok (Node3 (t1', j, t2, k, t3))
            | Split (t1a, i', t1b) ->  (* split nodes *)
              Split (Node2 (t1a, i', t1b), j, Node2 (t2, k, t3))
        end

    | Node3 (t1, j, t2, k, t3) when i > j && i < k ->  (* insert into t2 *)
        begin
          match insert_helper i t2 with
            | Ok t2' -> Ok (Node3 (t1, j, t2', k, t3))
            | Split (t2a, i', t2b) ->  (* split nodes *)
              Split (Node2 (t1, j, t2a), i', Node2 (t2b, k, t3))
        end

    | Node3 (t1, j, t2, k, t3) ->  (* i > k; insert into t3 *)
        begin
          match insert_helper i t3 with
            | Ok t3' -> Ok (Node3 (t1, j, t2, k, t3'))
            | Split (t3a, i', t3b) ->  (* split nodes *)
              Split (Node2 (t1, j, t2), k, Node2 (t3a, i', t3b))
        end

let tree_insert i t =
  match insert_helper i t with
    | Ok t' -> t'
    | Split (t1, j, t2) -> Node2 (t1, j, t2)