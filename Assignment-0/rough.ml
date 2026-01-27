let l=[1; 2; 3; 4];;
let rec print_list l = match l with
  | [x] -> print_int x
  | x :: xs -> print_int x; print_string " "; print_list xs
  | [] -> print_string "emptylist"
  ;;
let n=9;;
let var_id i j v =
  (* i,j are 1-based indices represent location in grid and v is the value in that location*)
  n*n*i +n*j + v;;
let l1=List.map (fun x -> x * x) l ;;
print_list l1;;


let a = [2;3;4];;

let rec print_list l = match l with
  | [x] -> print_int x
  | x :: xs -> print_int x; print_string " "; print_list xs
  | [] -> print_string "emptylist"
  ;;

print_list a;;


let clauses = ref [] in
for i = 1 to n do
  for j = 1 to n do
    let d = ref [] in
    for v = 1 to n do
      d := var_id i j v :: !d
    done;
    clauses := !d :: !clauses
  done
done;
!clauses