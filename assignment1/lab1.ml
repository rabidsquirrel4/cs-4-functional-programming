(* Author:          Rachel Shi 
   Assignment Due:  Jan 22, 2020 2 AM*)

(* A.1 *)

  (* 1. "10", result: "- : int = 10" *)
  (* 2. "10.", result: "- : float = 10." *)
  (* 3. "5 + 3 + 4", result: "- : int = 12" *)
  (* 4. "3.2 + 4.2", result: "Error: This expression has type 
        float but an expression was expected of type int", 
        The error occurred because the operator "+." should 
        be used with float expressions, not "+", which is used 
        for integers. *)
  (* 5. "3 +. 4", result: "Error: This expression has type 
        int but an expression was expected of type float", 
        The error occurred because the operator "+" should 
        be used with integer expressions, not "+.", which is 
        used for floats. *)
  (* 6. "3 + 4.2", result: "Error: This expression has type 
        float but an expression was expected of type int", 
        The error occurred because the operator "+" should 
        be used with only integer expressions, and 4.2 is
        a float expression. *)
  (* 7. "3 +. 4.2", result: "Error: This expression has type 
        int but an expression was expected of type float", 
        The error occurred because the operator "+." should 
        be used with only float expressions, and 3 is
        an integer expression. *)
  (* 8. "3.0 +. 4.2", result: "- : float = 7.2" *)
  (* 9. "9 - 3 - 1", result: "- : int = 5" *)
  (* 10. "9 - (3 - 1)", result: "- : int = 7" *)
  (* 11. "let a = 3", result: "val a : int = 3" *)
  (* 12. "let b = a + 1", result: "val b : int = 4" *)
  (* 13. "a = b", result: "- : bool = false" *)
  (* 14. "[1; 2; 3] = [1; 2; 3]", result: "- : bool = true" *)
  (* 15. "[1; 2; 3] == [1; 2; 3]", result: "- : bool = false",
          This is different from the previous expression because
          the "==" operator checks if two things have the same 
          pointer (location stored in memory) and the "=" operator
          checks if the expressions are equivalent. *)
  (* 16. "[(1, 2, 3)]", result: "- : (int * int * int) list = [(1, 2, 3)]" *)
  (* 17. "[1, 2, 3]", result: "- : (int * int * int) list = [(1, 2, 3)]",
          This gives a list with one 3-tuple entry because list elements 
          should be separated with semi-colons, and the parentheses for a 
          tuple can be omitted. Thus ocaml interprets the elements
          separated by commas to being in a tuple and that tuple is an
          element in the list. *)
  (* 18. "if b > a && b < a * b then b else a", result: "- : int = 4" *)
  (* 19. "if b > a and b < a * b then b else a", 
          result: "Error: Syntax error",
          Unlike python, "and" is not a correct operator in ocaml, so it is
          not recognized by the interpreter and causes a syntax error. *)
  (* 20. "2 + if b > a then b else a", result: "- : int = 6" *)
  (* 21. "if b > a then b else a + 2", result: "- : int = 4", 
          This is different from the previous case since the "+ 2" is 
          operating as part of the else statement in this case, as there 
          are no begin/ends to specify when the else statement is over. *)  
  (* 22. "(if b > a then b else a) + 2", result: "- : int = 6" *)  
  (* 23. "if b > a then b", result: "Error: This expression has type int but 
          an expression was expected of type unit because it is in the result 
          of a conditional with no else branch", 
          Ocaml gives a type error because if an else clause is omitted,
          ocaml expects the then clause to have an expression with type
          unit, and since b does not have type unit, ocaml gives a type 
          error. *)  

(* A.2 *)

let sum_of_squares_of_two_largest a b c = 
  if a > c && b > c then 
    a * a + b * b 
  else 
    (if b > a && c > a then 
       b * b + c * c 
     else a * a + c * c)

(* A.3 *)

  (* The function adds the absolute value of b to a and returns the sum.
     It does this by checking if b is positive. If b is positive the addition
     operator is used to add a to b. However if b is negative, b is subtracted
     from a, and thus the absolute value of b is added to a. The operator 
     used is dependent on the if/then/else clause. *)

(* B.1 *)

  (* Ben will observe infinite looping with an interpreter that uses 
     applicative-order evaluation, since applicative-order evaluation first
     attempts to evaluate the arguments before applying the expression. This
     is because the argument p () will continuously evaluate to itself,
     and thus the interpreter never gets to evaluating the full expression.
     With an interpreter that uses normal-order evaluation, the expression
     will evaluate to 0, because when the expression is expanded and then 
     reduced, the interpreter will quickly realize that y or p () does
     not need to be fully evaluated. *)

(* B.2 *)
  
  (* When Alyssa attempts to use this to compute square roots, she will
     get a type error. This is because guess is a float, while
     (sqrt_iter (improve guess x) x) is a function. Even though the
     function returns a float, the interpreter does not attempt to 
     evaluate it and simply sees it as the wrong type. This is why 
     if needs to be provided as a special syntactic form. *)
  
(* B.3 *)

  (* B.3.1 *)
  (* add_a generates a recursive processes because there is still 
      more computation to be done after add_a is recursively called.
      add_b generates an iterative processes because there are no 
      pending operations after the recursive call, and thus the 
      function can be tail call optimized. *)
  
  (* B.3.2 *)
  (* add_a substitution model evaluation
  let rec add_a a b =
    if a = 0
      then b
      else inc (add_a (dec a) b)
  
  - Desugar this to:

  let rec add_a = 
    fun a b ->
      if a = 0
        then b
        else inc (add_a (dec a) b)

  - Create function fun a b -> if ...
  - Bind name "add_a" to the value: 
    fun a b ->
      if a = 0
        then b
        else inc (add_a (dec a) b)

  - Evaluate (add_a 2 5)
    - Evaluate 2 -> 2
    - Evaluate 5 -> 5
    - Evaluate add_a -> to desugared function:
      fun a b ->
        if a = 0
          then b
          else inc (add_a (dec a) b)
    - Apply fun a b ->
      if a = 0 
        then b
        else inc (add_a (dec a) b)
    to 2, 5.
    - Substitute 2 for a and 5 for b in fun expression to get:
      ->if 2 = 0 
          then 5 
          else inc (add_a (dec 2) 5)
    - Evaluate: if 2 = 0 then 5 else inc (add_a (dec 2) 5)
      - if is a special form, so evaluate the first operand:
      - Evaluate 2 = 0
        - Evaluate 2 -> 2
        - Evaluate 0 -> 0
        - Evaluate = -> =
        - Apply = to 2, 0 -> false
      - Since the expression is false, replace with false clause:
        inc (add_a (dec 2) 5)
      - Evaluate: inc (add_a (dec 2) 5)
    - Evaluate: inc (add_a (dec 2) 5)
      - Evaluate (add_a (dec 2) 5)
        - Evaluate dec 2
          - Evaluate 2 -> 2
          - Evaluate dec -> dec
          - Apply dec to 2 -> 1
        - Evaluate 5 -> 5
        - Evaluate add_a -> 
          fun a b -> if ...
        - Apply add_a -> (fun a b -> ...) to 1, 5
          - Substitute 1 for a and 5 for b in the fun expression to get:
            ->if 1 = 0 
                then 5 
                else inc (add_a (dec 1) 5)
          - Evaluate: if 1 = 0 then 5 else inc (add_a (dec 1) 5)
            - if is a special form, so evaluate the first operand:
            - Evaluate 1 = 0
              - Evaluate 1 -> 1
              - Evaluate 0 -> 0
              - Evaluate = -> = 
              - Apply = to 1, 0 -> false
            - Since the expression is false, replace with the false clause:
                inc (add_a (dec 1) 5)
            - Evaluate: inc (add_a (dec 1) 5)
          - Evaluate: inc (add_a (dec 1) 5)
            - Evaluate (add_a (dec 1) 5)
              - Evaluate dec 1
                - Evaluate 1 -> 1
                - Evaluate dec -> dec
                - Apply dec to 1 -> 0
              - Evaluate 5 -> 5
              - Evaluate add_a -> 
                fun a b -> if ...
              - Apply add_a -> (fun a b -> ...) to 0, 5
                - Substitute 0 for a and 5 for b in the fun expression to get:
                  ->if 0 = 0 
                      then 5 
                      else inc (add_a (dec 0) 5)
                - Evaluate: if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                  - if is a special form, so evaluate the first operand:
                  - Evaluate 0 = 0
                    - Evaluate 0 -> 0
                    - Evaluate 0 -> 0
                    - Evaluate = -> =
                    - Apply = to 0, 0 -> true
                  - Since the expression is true, replace with the true clause:
                      5
                  - Evaluate 5 -> 5
                  - result: 5
            - Evaluate inc -> inc
            - Apply inc to 5 -> 6
            - result: 6
      - Evaluate inc -> inc
      - Apply inc to 6 -> 7
    - final result: 7
  *)
  
  (* B.3.3 *)

  (*
  let rec add_b a b =
    if a = 0
      then b
      else add_b (dec a) (inc b)

  Desugar this to:

  let rec add_b =
    fun a b ->
      if a = 0
        then b
        else add_b (dec a) (inc b)

  >>> Create function fun a b -> if ...
  Bind the name "add_b" to the value:

    fun a b ->
      if a = 0
        then b
        else add_b (dec a) (inc b)

  Evaluate (add_b 2 5)
    >>> evaluate 2 -> 2
    >>> evaluate 5 -> 5
    >>> evaluate add_b -> (fun a b -> if a = 0 then b else ...)
    apply (fun a b -> if ...) to 2, 5
    substitute 2 for a, 5 for b in (if ...)
      -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
    evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
      if is a special form, so evaluate the first operand:
        evaluate (2 = 0)
          >>> evaluate 2 -> 2
          >>> evaluate 0 -> 0
          >>> evaluate = -> =
          apply = to 2, 0 -> false
      first argument of if is false, so evaluate the third operand:
        evaluate (add_b (dec 2) (inc 5))
          evaluate (dec 2)
            >>> evaluate 2 -> 2
            >>> evaluate dec -> dec
            apply dec to 2 -> 1
          evaluate (inc 5)
            >>> evaluate 5 -> 5
            >>> evaluate inc -> inc
            apply inc to 5 -> 6
          apply (fun a b -> if ...) to 1, 6
          substitute 1 for a, 6 for b in (if ...)
            -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
          evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
            if is a special form, so evaluate the first operand:
              evaluate (1 = 0)
                >>> evaluate 1 -> 1
                >>> evaluate 0 -> 0
                >>> evaluate = -> =
                apply = to 1, 0 -> false
            first argument of if is false, so evaluate the third operand:
              evaluate (add_b (dec 1) (inc 6))
                evaluate (dec 1)
                  >>> evaluate 1 -> 1
                  >>> evaluate dec -> dec
                  apply dec to 1 -> 0
                evaluate (inc 6)
                  >>> evaluate 6 -> 6
                  >>> evaluate inc -> inc
                  apply inc to 6 -> 7
                apply (fun a b -> if ...) to 0, 7
                substitute 0 for a, 7 for b in (if ...)
                  -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
                evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                  if is a special form, so evaluate the first operand:
                    evaluate (0 = 0)
                      >>> evaluate 0 -> 0
                      >>> evaluate 0 -> 0
                      >>> evaluate = -> =
                      apply = to 0, 0 -> true
                  first argument of if is true, so evaluate the second operand:
                    >>> evaluate 7 -> 7
                    result: 7
  *)

(* C.1 *)

(* This function computes the factorial of the input number,
   which for a number n is equal to n * (n-1) * ... * 1. *)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)

  (* C.1.a *)

(* This function computes a particular term in the infinite 
  series expansion of e. *)
let e_term n =
  if n < 0
    then 0.0
    else 1.0 /. (float_of_int (factorial n))

  (* C.1.b *)

let rec e_approximation n =
  if n <= 0
    then e_term 0
    else (e_term n) +. e_approximation (n - 1)

  (* C.1.c *)

    (* Result from e_approximation 20: 
        - : float = 2.71828182845904553 *)
    (* Value of exp 1.0: 
        - : float = 2.71828182845904509*)
  
  (* C.1.d *)
    (* If we try to compute a better approximation of e by summing 
       up to the 100th term we get: 
       - : float = infinity
      This happens because the factorial becomes so large that 
      (factorial 100) simply returns 0. Then when 1.0 is divided by 
      zero, it adds infinity to the sum and thus e_approximation of 
      100 returns infinity.
    *)

(* C.2 *)

let rec is_even n =
  if n = 0 
    then true
    else is_odd (n - 1)
and is_odd n = 
  if n = 0
    then false
    else is_even (n - 1)

(* C.3 *)

let rec f_rec n = 
  if n < 3
    then n
    else f_rec (n - 1) + 2 * f_rec (n - 2) + 3 * f_rec (n - 3)

let rec f_iter_helper n current a b c =
  if current + 2 >= n
    then a
    else f_iter_helper n (current + 1) (a + 2 * b + 3 * c) a b

let f_iter n = 
  if n < 3
    then n
    else f_iter_helper n 0 2 1 0

(* C.4 *)         

let rec pascal_coefficient row idx =  
  match (row, idx) with
    | (row', idx') when (row' < 1 || idx' < 1 || row' < idx') 
      -> failwith "invalid arguments"
    | (row', idx') when row' = idx' -> 1
    | (_, 1) -> 1
    | _ -> pascal_coefficient (row - 1) idx
            + pascal_coefficient (row - 1) (idx - 1)
