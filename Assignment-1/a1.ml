let exists s x = List.fold_left (fun acc (y) -> acc || (y=x) ) false s

module Signature =struct
  type symbol = string*int
  type signature = symbol list

  let rec arities_check s=
    match s with
    | []-> true
    | (_, a)::rest -> (a>=0) && arities_check rest

  let exists s (x,_) = List.fold_left (fun acc (y,_) -> acc || (y=x) ) false s

  let rec repetitions s=
    match s with
    | [] -> false
    | x::rest -> exists rest x|| repetitions rest
  
  let check_sig (s : signature) : bool =
    let ac = arities_check s in
    let rep = repetitions s in
    ac && not rep
end

module Expression =struct
  type variable =string
  type exp =V of variable
           |Node of Signature.symbol * (exp array)
  
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
    | V _ ->1
    | Node ((name, arity), children) ->
      1+ Array.fold_left (fun acc x -> acc+x) 0 (Array.map size children)

  let dedup arr = (* removes duplicates from array *)
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


  let lookup (s : substitution) (v : variable) : exp = Array.fold_left (fun acc (a, b) -> if a = v then b else acc) (V v) s
    
  let rec subst (s : substitution) (e : exp) :exp =
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

  let rec inplace_subst (s: substitution) (e : exp) : unit = (* neglecting the case where node itself is a variable *)
    match e with
    | V v -> ()
    | Node (symbol, children) ->
      Array.iteri (fun i child -> match child with | V v -> children.(i)<- lookup s v | Node(sym,ch) -> inplace_subst s child) children
end



module Predicate =struct
  type pred_symbol = string * int
  type pred_signature = pred_symbol list
  type pred = T | F  | Pred of pred_symbol * (Expression.exp array) |  Not of pred | And of pred * pred | Or of pred * pred

  let rec wff (s : Signature.signature) (ps : pred_signature) (p : pred) : bool =
    match p with
    | T  -> true
    | F -> true
    | Not x -> wff s ps x
    | And (a, b) -> wff s ps a && wff s ps b
    | Or (c, d) -> wff s ps c && wff s ps d
    | Pred ((name, arity), exp_array) -> exists ps (name, arity) && arity = Array.length exp_array && Array.for_all (Expression.wfexp s) exp_array

  let rec psubst (sub : Expression.substitution) (p : pred) : pred =
    match p with
    | T -> T
    | F -> F
    | Not x -> Not (psubst sub x)
    | And (a, b) -> And (psubst sub a, psubst sub b)
    | Or (c, d) -> Or (psubst sub c, psubst sub d)
    | Pred ((name, arity), exp_array) -> Pred ((name, arity), Array.map (Expression.subst sub) exp_array)

  let wp (x : Expression.variable) (e : Expression.exp) (p : pred) : pred = psubst [|(x, e)|] p
 
end

let check_sig = Signature.check_sig
let wfexp = Expression.wfexp
let ht = Expression.ht
let size = Expression.size
let vars = Expression.vars
let subst = Expression.subst
let compose = Expression.compose
let edit = Expression.edit
let inplace_subst = Expression.inplace_subst
let wff = Predicate.wff
let psubst = Predicate.psubst
let wp = Predicate.wp