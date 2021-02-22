(* Author:          Rachel Shi 
   Assignment Due:  Feb 26, 2020 2 AM *)

(*** Part A. ***)

(* A.1 *)
let fibonacci n = 
  begin
    let result = ref 0 in
    let next = ref 1 in
    let i = ref n in
    while !i > 0 do
      let temp = !result + !next in
      result := !next;
      next := temp;
      i := !i - 1
    done;
    !result
  end

let fibonacci2 n=
  begin
    let result = ref 0 in
    let next = ref 1 in
    for i = 1 to n do
      let temp = !result + !next in
      result := !next;
      next := temp
    done;
    !result
  end
  
(* A.2 *)
let bubble_sort arr =
  begin
    let len = Array.length arr and
    finished = ref false in
    while not !finished do 
      finished := true;
      for i = 0 to len - 2 do
        if arr.(i) > arr.(i + 1)
          then 
          let temp = arr.(i) in 
          arr.(i) <- arr.(i + 1);
          arr.(i + 1) <- temp; 
          finished := false;
      done;
    done;
  end

(*** Part B. ***)

(* B.1 *)
let meters_per_foot = 0.3048
(* let inches_per_foot = 12.0 *)

let get_meters len =
  match len with
    | `Meter m -> m
    | `Foot f -> f *. meters_per_foot
    | `Inch i -> (i /. 12.0) *. meters_per_foot

let length_add a b = `Meter (get_meters a +. get_meters b)

(* B.2 *)
let grams_per_slug = 14593.903203

let get_grams weight = 
  match weight with
  | `Gram g -> g
  | `Kilo k -> (k *. 1000.0)
  | `Slug s -> s *. grams_per_slug

let mass_add a b = `Gram (get_grams a +. get_grams b)

let get_seconds time = 
  match time with
    | `Second s -> s
    | `Minute m -> m *. 60.0 
    | `Hour h -> h *. 60.0 *. 60.0
    | `Day d -> d *. 24.0 *. 60.0 *. 60.0

let time_add a b = `Second (get_seconds a +. get_seconds b)

(* B.3 *)
let unit_add a b = 
  match (a, b) with 
    | (`Length a', `Length b') -> length_add a' b'
    | (`Mass a', `Mass b') -> mass_add a' b'
    | (`Time a', `Time b') -> time_add a' b'
    | (_, _) -> failwith "units incompatible"

(* We don't get a combinatorial explosion when adding more unit classes
    with unit addition because we only have to add units that are 
    compatible. Each time we add a new unit class, we only have to add when both
    values given are of that unit class, otherwise we only need to default to 
    the failwith case. For each unit class added to unit_add, we only need 
    to add one new case. Thus, there is no combinatorial explosion. *)

(*** Part C. ***)

(* C.1 *)
let rec make_gram g =
  let
    ... (* internal definitions *)
  in
    object
      method get_grams = ...
      method get_slugs = ...
      method unit_type = ...
      method compatible other = ...
      method add other = ...
    end

(* C.2 *)

val make_number :
  int ->
  (< 
     derive    : string -> 'a; 
     evaluate  : string -> int -> 'a; 
     is_number : bool;
     is_zero   : bool; 
     show      : string; 
     value     : int 
   >
   as 'a)

val make_variable :
  string ->
  (< 
     derive    : string -> 'a; 
     evaluate  : string -> int -> 'a; 
     is_number : bool;
     is_zero   : bool; 
     show      : string; 
     value     : int 
   >
   as 'a)

val make_sum :
  (< 
     derive    : string -> 'a; 
     evaluate  : string -> int -> 'a; 
     is_number : bool;
     is_zero   : bool; 
     show      : string; 
     value     : int 
   >
   as 'a) ->
  'a -> 'a

val make_product :
  (< 
     derive    : string -> 'a; 
     evaluate  : string -> int -> 'a; 
     is_number : bool;
     is_zero   : bool; 
     show      : string; 
     value     : int 
   >
   as 'a) ->
  'a -> 'a

val evaluate : < evaluate : 'a -> 'b -> 'c; .. > -> 'a -> 'b -> 'c

val show : < show : 'a; .. > -> 'a

val differentiate : < derive : 'a -> 'b; .. > -> 'a -> 'b
