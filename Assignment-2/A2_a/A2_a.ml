module BIGNUM =struct
  type sign = Neg| NonNeg
  type bigint = sign * int list
  type myBool = T| F
  exception Division_by_zero of string

  let rec reverse_list k ans = match k with
    | [] -> ans
    | p :: ps -> reverse_list ps (p::ans)

  let reverse_digits (x : bigint) = match x with
    | (_,l) -> reverse_list l []
    
  let negate (x : bigint) = match x with
    | (_,[0]) -> (NonNeg,[0])
    | (s,l) -> if s=Neg then NonNeg,l
               else Neg,l

  let absolute (x : bigint) = match x with
    | (s,l) -> if s=Neg then NonNeg,l else x 
  
  let rec strip_trailing_zeroes x = match x with
    | 0 :: xs -> if (List.length x >1) then strip_trailing_zeroes xs else [0]
    | _ -> x

(* add and sub *)

  let rec add_helper al bl carry ans = match al,bl with
    | (a::ar),(b::br) -> add_helper ar br ((a+b+carry)/10) (((a+b+carry) mod 10) :: ans)
    | [],[] -> if(carry=0) then ans else 1::ans
    | [], k::kr -> add_helper [] kr ((k+carry)/10) (((k+carry) mod 10) ::ans)
    | k::kr, [] -> add_helper kr [] ((k+carry)/10) (((k+carry) mod 10) ::ans)

  let rec sub_helper al bl borrow ans = match al,bl with
    | [], [] -> ans
    | a::ar, b::br -> let diff = a - b - borrow in
                       if diff < 0 
                       then sub_helper ar br 1 ((diff+10)::ans)
                       else sub_helper ar br 0 (diff::ans)
    | k::kr, [] -> let diff = k - borrow in
                   if diff < 0
                   then sub_helper kr [] 1 ((diff+10)::ans)
                   else sub_helper kr [] 0 (diff::ans)
    | [], _ -> ans (* This case is not even possible , since length of al>= bl *)

  let adder a b = (NonNeg,add_helper (reverse_digits a) (reverse_digits b) 0 [])

  let rec subtractor a b = 
    let al = match a with (_, l) -> l in
    let bl = match b with (_, l) -> l in
    let la = List.length al and lb = List.length bl in
    if (la > lb) || (la = lb && al >= bl) then
      (NonNeg, strip_trailing_zeroes (sub_helper (reverse_digits a) (reverse_digits b) 0 []))
    else
      negate (NonNeg, strip_trailing_zeroes (sub_helper (reverse_digits b) (reverse_digits a) 0 []))

  let add (a : bigint) (b : bigint) = match (a,b) with
    | (s1,al),(s2,bl) -> if (s1=NonNeg && s2=NonNeg) then adder a b
                         else if (s1=Neg && s2=NonNeg) then subtractor b (negate a)
                         else if (s2=Neg && s1=NonNeg) then subtractor a (negate b)
                         else negate (adder (negate a) (negate b))

  let sub (a : bigint) (b : bigint)  = add a (negate b)
  
  let rec itb_helper x =
    if (x>=0 && x<=9) then [x] else (x mod 10)::itb_helper (x/10)


  let int_to_bigint x =
    let s = if(x>=0) then NonNeg else Neg in
    let l = reverse_list (itb_helper (abs(x))) [] in
    (s,l)

(* mult *)

  let padding_zeroes l x = 
    let rec pad_help l x = match x with
    | 0 -> l
    | _ -> pad_help (0::l) (x-1)
  in reverse_list (pad_help (reverse_list l []) x) []
  
  let mult_single l x =
    let rec mult_sin l x carry ans= match l with
      | [] -> if carry = 0 then ans else carry :: ans
      | k ::ks -> (mult_sin ks x (((k*x)+carry)/10) (((k*x)+carry) mod 10 :: ans))
  in (NonNeg, strip_trailing_zeroes(mult_sin l x 0 []))
    
  let mult_helper al bl len =
    let rec mult_help al bl ans len = match bl with
      | b :: bs -> 
        let (_, m) = mult_single al b in
        mult_help al bs ((NonNeg, padding_zeroes m len) :: ans) (len+1)
      | [] -> ans
  in  List.fold_left add (NonNeg,[0]) (mult_help al bl [] len)

  let mult (a : bigint) (b : bigint) =
    let (s1, al) = a and (s2, bl) = b in
    let result = mult_helper (reverse_list al []) (reverse_list bl []) 0 in
    if s1 = s2 then result else negate result
        
(* Quotient and remainder *)
  let find_q current divisor =
  let rec try_q q =
    if q > 9 then 9
    else
      let (_, prod) = mult_single (reverse_list divisor []) q in
      let result = subtractor (NonNeg, current) (NonNeg, prod) in
      match result with
      | (Neg, _) -> (q - 1)
      | _ -> if q = 9 then 9 else try_q (q + 1)
  in try_q 1

  let div_helper dividend divisor =
    let rec help remaining current quot_acc = match remaining with
      | [] -> (strip_trailing_zeroes quot_acc, current)
      | d :: ds ->
        let current' = strip_trailing_zeroes (current @ [d]) in
        let q = find_q current' divisor in
        let (_, prod) = mult_single (reverse_list divisor []) q in
        let (_, rem) = subtractor (NonNeg, current') (NonNeg, prod) in
        help ds rem (quot_acc @ [q])
    in help dividend [0] []

  let div (a : bigint) (b : bigint) = match a,b with
    | (s1, al),(s2, bl) ->
      if bl = [0] then raise (Division_by_zero "Division by zero is not possible")
      else
        let (q, _) = div_helper al bl in
        if s1 = s2 then (NonNeg, q) else negate (NonNeg, q) (* since q can be 0 in which case we are not writing neg 0 *)

  let modulo (a : bigint) (b : bigint) = match a,b with
    | (s1, al),(s2, bl) ->
      if bl = [0] then raise (Division_by_zero "Division by zero is not possible")
      else
        let (_, r) = div_helper al bl in
        (s1, r)

(* comparisions *)
  let rec equal_helper al bl = match al,bl with
    | (x::xs),(y::ys) -> 
      if(x=y) then equal_helper xs ys else F
    | _,_ -> T

  let equal (a : bigint) (b : bigint) : myBool = match a,b with
    | (s1,al),(s2,bl) ->
      if (not (s1=s2)) then F
      else if (not (List.length al = List.length bl)) then F
      else equal_helper al bl

  
  let rec gt_helper al bl = match al,bl with
    | (x::xs),(y::ys) ->
      if(x>y) then T
      else if (x<y) then F
      else gt_helper xs ys
    | _,_ -> F

  let greater_than (a : bigint) (b : bigint) : myBool = match a,b with
    | (s1,al),(s2,bl) ->
      if (s1 = Neg && s2 =NonNeg) then F
      else if (s1 =NonNeg && s2 = Neg) then T
      else if (List.length al < List.length bl) && (s1=NonNeg) then F
      else if (List.length al > List.length bl) && (s1=NonNeg) then T
      else if (List.length al < List.length bl) && (s1=Neg) then T
      else if (List.length al > List.length bl) && (s1=Neg) then F
      else if (s1 = NonNeg) then gt_helper al bl
      else gt_helper bl al
  
  let less_than (a : bigint) (b : bigint) : myBool = 
  if greater_than a b = T || equal a b = T then F else T

  let greater_or_equal (a : bigint) (b : bigint) : myBool = 
    if greater_than a b = T || equal a b = T then T else F

  let less_or_equal (a : bigint) (b : bigint) : myBool = 
    if less_than a b = T || equal a b = T then T else F

  let print_answer (s, l) =
    let digit_str = String.concat "" (List.map string_of_int l) in
    if s = Neg then "-" ^ digit_str else digit_str
end

let()=
  let a =BIGNUM.int_to_bigint (-5) in
  let b =BIGNUM.int_to_bigint (4) in
  print_endline (BIGNUM.print_answer (BIGNUM.div a b))