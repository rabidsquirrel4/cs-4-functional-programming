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

  (* *)
  (* 1. add_a generates a recursive processes because there is still 
        more computation to be done after add_a is recursively called.
        add_b generates an iterative processes because there are no 
        pending operations after the recursive call, and thus the 
        function can be tail call optimized. *)
  
  (* 2. add_a substitution model evaluation
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
        - Evaluate 
      - Evaluate inc -> 
  *)
  
  (* 3. *)

(* C.1 *)

(* C.2 *)

(* C.3 *)

(* C.4 *)         