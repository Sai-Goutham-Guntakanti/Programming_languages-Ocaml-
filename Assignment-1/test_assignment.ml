let exists s x = List.fold_left (fun acc (y) -> acc || (y=x) ) false s

module Signature = struct
  type symbol = string*int
  type signature = symbol list

  let rec arities_check s =
    match s with
    | [] -> true
    | (_, a)::rest -> (a>=0) && arities_check rest

  let exists s (x,_) = List.fold_left (fun acc (y,_) -> acc || (y=x) ) false s

  let rec repetitions s =
    match s with
    | [] -> false
    | x::rest -> exists rest x || repetitions rest
  
  let check_sig (s:signature) : bool =
    let ac = arities_check s in
    let rep = repetitions s in
    ac && not rep
end

module Expression = struct
  type variable = string
  type exp = V of variable
           | Node of Signature.symbol * (exp array)
  
  let rec wfexp (s : Signature.signature) (e : exp) : bool =
    match e with
    | V _ -> true
    | Node ((name, arity), children) ->
      exists s (name, arity)
      && Array.length children = arity
      && Array.for_all (wfexp s) children

  let rec ht (e : exp) : int =
    match e with
    | V _ -> 0
    | Node ((name, arity), children) ->
      if(arity=0) then 0
      else 1+ Array.fold_left max 0 (Array.map ht children)

  let rec size (e : exp) : int =
    match e with
    | V _ -> 1
    | Node ((name, arity), children) ->
      1 + Array.fold_left (fun acc x -> acc+x) 0 (Array.map size children)

  let dedup arr =
    Array.fold_left (fun acc x ->
      if Array.exists (fun y -> y = x) acc then acc
      else Array.append acc [|x|]
    ) [||] arr

  let rec vars (e : exp) : variable array =
    match e with
    | V x -> [|x|]
    | Node ((name, arity), children) ->
      dedup (Array.fold_left (fun acc child -> Array.append acc (vars child)) [||] children)
  
  type substitution = (variable * exp) array

  let lookup (s : substitution) (v : variable) : exp = 
    Array.fold_left (fun acc (a, b) -> if a = v then b else acc) (V v) s
    
  let rec subst (s : substitution) (e : exp) : exp =
    match e with
    | V x -> lookup s x
    | Node (symbol, children) ->
      Node (symbol, Array.map (subst s) children)

  let compose (s1 : substitution) (s2 : substitution) : substitution =
    let updated_s1 = Array.map (fun (x, e) -> (x, subst s2 e)) s1 in
    let s2_extra = Array.of_list
      (List.filter (fun (x, _) -> not (Array.exists (fun (y, _) -> y = x) s1)) (Array.to_list s2)) in
    Array.append updated_s1 s2_extra

  type position = int list

  let rec edit (e : exp) (p : position) (e_new : exp) : exp =
    match p with
    | [] -> e_new
    | i :: rest ->
      (match e with
      | V _ -> failwith "Not possible"
      | Node (symbol, children) ->
        let new_children = Array.copy children in
        new_children.(i) <- edit (children.(i)) rest e_new;
        Node (symbol, new_children))

  let rec inplace_subst (s: substitution) (e : exp) : unit =
    match e with
    | V v -> ()
    | Node (symbol, children) ->
      Array.iteri (fun i child -> match child with 
        | V v -> children.(i) <- lookup s v 
        | Node(sym,ch) -> inplace_subst s child) children
end

module Predicate = struct
  type pred_symbol = string * int
  type pred_signature = pred_symbol list
  type pred = T | F | Pred of pred_symbol * (Expression.exp array) | Not of pred | And of pred * pred | Or of pred * pred

  let rec wff (s : Signature.signature) (ps : pred_signature) (p : pred) : bool =
    match p with
    | T -> true
    | F -> true
    | Not x -> wff s ps x
    | And (a, b) -> wff s ps a && wff s ps b
    | Or (c, d) -> wff s ps c && wff s ps d
    | Pred ((name, arity), exp_array) -> 
      exists ps (name, arity) && arity = Array.length exp_array && Array.for_all (Expression.wfexp s) exp_array

  let rec psubst (sub : Expression.substitution) (p : pred) : pred =
    match p with
    | T -> T
    | F -> F
    | Not x -> Not (psubst sub x)
    | And (a, b) -> And (psubst sub a, psubst sub b)
    | Or (c, d) -> Or (psubst sub c, psubst sub d)
    | Pred ((name, arity), exp_array) -> Pred ((name, arity), Array.map (Expression.subst sub) exp_array)

  let wp (x : Expression.variable) (e : Expression.exp) (p : pred) : pred = 
    psubst [|(x, e)|] p
end


(* ==================== TESTS ==================== *)

open Expression
open Predicate

let () = print_endline "=== SIGNATURE TESTS ==="

(* Test check_sig *)
let sig1 = [("f", 2); ("g", 1); ("c", 0)]
let () = assert (Signature.check_sig sig1 = true);          print_endline "check_sig valid sig: PASS"
let () = assert (Signature.check_sig [] = true);             print_endline "check_sig empty sig: PASS"
let () = assert (Signature.check_sig [("f", 2); ("f", 3)] = false);  print_endline "check_sig duplicate name: PASS"
let () = assert (Signature.check_sig [("f", -1)] = false);  print_endline "check_sig negative arity: PASS"
let () = assert (Signature.check_sig [("f", 2); ("g", 2)] = true);   print_endline "check_sig same arity diff name: PASS"


let () = print_endline "\n=== EXPRESSION TESTS ==="

(* Some expressions for testing *)
let x = V "x"
let y = V "y"
let z = V "z"
let const_c = Node (("c", 0), [||])
let const_d = Node (("d", 0), [||])  (* not in sig1 *)
let f_xy = Node (("f", 2), [| x; y |])
let g_x = Node (("g", 1), [| x |])
let f_gx_y = Node (("f", 2), [| g_x; y |])
let f_gx_fxy = Node (("f", 2), [| g_x; f_xy |])

(* wfexp tests *)
let () = assert (wfexp sig1 x = true);                      print_endline "wfexp variable: PASS"
let () = assert (wfexp sig1 const_c = true);                 print_endline "wfexp constant: PASS"
let () = assert (wfexp sig1 f_xy = true);                    print_endline "wfexp f(x,y): PASS"
let () = assert (wfexp sig1 g_x = true);                     print_endline "wfexp g(x): PASS"
let () = assert (wfexp sig1 f_gx_y = true);                  print_endline "wfexp f(g(x),y): PASS"
let () = assert (wfexp sig1 f_gx_fxy = true);                print_endline "wfexp nested: PASS"

(* wfexp should fail *)
let bad1 = Node (("f", 2), [| x |])                         (* f has arity 2 but 1 child *)
let bad2 = Node (("h", 1), [| x |])                         (* h not in signature *)
let bad3 = Node (("f", 2), [| x; bad2 |])                   (* child is bad *)
let () = assert (wfexp sig1 bad1 = false);                   print_endline "wfexp wrong arity: PASS"
let () = assert (wfexp sig1 bad2 = false);                   print_endline "wfexp unknown symbol: PASS"
let () = assert (wfexp sig1 bad3 = false);                   print_endline "wfexp bad child: PASS"

(* ht tests *)
let () = assert (ht x = 0);                                  print_endline "ht variable: PASS"
let () = assert (ht const_c = 0);                            print_endline "ht constant: PASS"
let () = assert (ht f_xy = 1);                               print_endline "ht f(x,y): PASS"
let () = assert (ht g_x = 1);                                print_endline "ht g(x): PASS"
let () = assert (ht f_gx_y = 2);                             print_endline "ht f(g(x),y): PASS"
let () = assert (ht f_gx_fxy = 2);                           print_endline "ht f(g(x),f(x,y)): PASS"

(* size tests *)
let () = assert (size x = 1);                                print_endline "size variable: PASS"
let () = assert (size const_c = 1);                          print_endline "size constant: PASS"
let () = assert (size f_xy = 3);                             print_endline "size f(x,y): PASS"
let () = assert (size g_x = 2);                              print_endline "size g(x): PASS"
let () = assert (size f_gx_y = 4);                           print_endline "size f(g(x),y): PASS"
let () = assert (size f_gx_fxy = 6);                         print_endline "size f(g(x),f(x,y)): PASS"

(* vars tests *)
let () = assert (vars x = [|"x"|]);                          print_endline "vars variable: PASS"
let () = assert (vars const_c = [||]);                       print_endline "vars constant: PASS"
let () = assert (vars f_xy = [|"x"; "y"|]);                  print_endline "vars f(x,y): PASS"
let () = assert (vars f_gx_y = [|"x"; "y"|]);               print_endline "vars f(g(x),y): PASS"
(* vars with duplicates *)
let f_xx = Node (("f", 2), [| x; x |])
let () = assert (vars f_xx = [|"x"|]);                       print_endline "vars dedup: PASS"


let () = print_endline "\n=== SUBSTITUTION TESTS ==="

(* subst tests *)
let s1 = [| ("x", const_c) |]                                (* x -> c *)
let s2 = [| ("x", y); ("y", z) |]                            (* x -> y, y -> z *)

let () = assert (subst s1 x = const_c);                      print_endline "subst x->c on x: PASS"
let () = assert (subst s1 y = y);                             print_endline "subst x->c on y: PASS"
let () = assert (subst s1 f_xy = Node (("f", 2), [| const_c; y |]));  print_endline "subst x->c on f(x,y): PASS"
let () = assert (subst s1 f_gx_y = Node (("f", 2), [| Node (("g", 1), [| const_c |]); y |])); 
                                                              print_endline "subst x->c on f(g(x),y): PASS"
let () = assert (subst s2 f_xy = Node (("f", 2), [| y; z |])); print_endline "subst simultaneous: PASS"

(* identity substitution *)
let id_sub = [||]
let () = assert (subst id_sub f_xy = f_xy);                  print_endline "subst identity: PASS"


let () = print_endline "\n=== COMPOSE TESTS ==="

(* compose: s1 then s2 *)
let sa = [| ("x", V "y") |]          (* x -> y *)
let sb = [| ("y", const_c) |]        (* y -> c *)
let sc = compose sa sb                (* should map x -> c, y -> c *)
let () = assert (subst sc (V "x") = const_c);               print_endline "compose x->y then y->c, apply to x: PASS"
let () = assert (subst sc (V "y") = const_c);               print_endline "compose x->y then y->c, apply to y: PASS"
let () = assert (subst sc (V "z") = V "z");                 print_endline "compose apply to z (unchanged): PASS"


let () = print_endline "\n=== EDIT TESTS ==="

(* edit tests *)
let () = assert (edit f_xy [] const_c = const_c);           print_endline "edit root: PASS"
let () = assert (edit f_xy [0] z = Node (("f", 2), [| z; y |]));  print_endline "edit first child: PASS"
let () = assert (edit f_xy [1] z = Node (("f", 2), [| x; z |]));  print_endline "edit second child: PASS"
let () = assert (edit f_gx_y [0; 0] z = Node (("f", 2), [| Node (("g", 1), [| z |]); y |]));
                                                              print_endline "edit nested: PASS"

let () = print_endline "\n=== INPLACE_SUBST TESTS ==="

(* inplace_subst test *)
let test_tree = Node (("f", 2), [| V "x"; V "y" |])
let () = inplace_subst [| ("x", const_c) |] test_tree
let () = assert (test_tree = Node (("f", 2), [| const_c; V "y" |]));  print_endline "inplace_subst: PASS"

let test_tree2 = Node (("f", 2), [| Node (("g", 1), [| V "x" |]); V "x" |])
let () = inplace_subst [| ("x", const_c) |] test_tree2
let () = assert (test_tree2 = Node (("f", 2), [| Node (("g", 1), [| const_c |]); const_c |]));  
                                                              print_endline "inplace_subst nested: PASS"


let () = print_endline "\n=== PREDICATE TESTS ==="

let pred_sig = [("P", 2); ("Q", 1)]

(* wff tests *)
let p1 = Pred (("P", 2), [| x; y |])
let p2 = Pred (("Q", 1), [| x |])
let p3 = And (p1, p2)
let p4 = Or (Not p1, p2)
let p_bad1 = Pred (("P", 2), [| x |])                       (* wrong arity *)
let p_bad2 = Pred (("R", 1), [| x |])                       (* R not in pred_sig *)

let () = assert (wff sig1 pred_sig p1 = true);              print_endline "wff Pred P(x,y): PASS"
let () = assert (wff sig1 pred_sig p2 = true);              print_endline "wff Pred Q(x): PASS"
let () = assert (wff sig1 pred_sig p3 = true);              print_endline "wff And: PASS"
let () = assert (wff sig1 pred_sig p4 = true);              print_endline "wff Or(Not,Pred): PASS"
let () = assert (wff sig1 pred_sig T = true);               print_endline "wff T: PASS"
let () = assert (wff sig1 pred_sig F = true);               print_endline "wff F: PASS"
let () = assert (wff sig1 pred_sig p_bad1 = false);         print_endline "wff wrong arity: PASS"
let () = assert (wff sig1 pred_sig p_bad2 = false);         print_endline "wff unknown pred symbol: PASS"

(* wff with bad expression inside *)
let p_bad_exp = Pred (("P", 2), [| x; Node(("h", 1), [| x |]) |])  (* h not in sig1 *)
let () = assert (wff sig1 pred_sig p_bad_exp = false);      print_endline "wff bad exp inside: PASS"

(* psubst tests *)
let sub1 = [| ("x", const_c) |]
let () = assert (psubst sub1 T = T);                        print_endline "psubst T: PASS"
let () = assert (psubst sub1 F = F);                        print_endline "psubst F: PASS"
let () = assert (psubst sub1 p1 = Pred (("P", 2), [| const_c; y |]));  print_endline "psubst Pred: PASS"
let () = assert (psubst sub1 (Not p1) = Not (Pred (("P", 2), [| const_c; y |])));  print_endline "psubst Not: PASS"
let () = assert (psubst sub1 p3 = And (Pred (("P", 2), [| const_c; y |]), Pred (("Q", 1), [| const_c |])));
                                                              print_endline "psubst And: PASS"

(* wp tests *)
let () = assert (wp "x" const_c p1 = Pred (("P", 2), [| const_c; y |]));  print_endline "wp: PASS"
let () = assert (wp "z" const_c p1 = p1);                   print_endline "wp unchanged: PASS"


let () = print_endline "\n=== ALL TESTS PASSED ==="