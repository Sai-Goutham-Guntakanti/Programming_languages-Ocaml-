let lines = In_channel.with_open_text Sys.argv.(1) In_channel.input_lines

let is_unsat = (* If it has "Unsat" in it then it has no solution*)
  List.exists (fun line ->  (* exists function returns true if atleast one of the list(here "lines") elems return true, otherwise returns false.*)
    let line_lower = String.lowercase_ascii line in
    let rec has_unsat i =
      if i + 5 > String.length line_lower then false
      else if String.sub line_lower i 5 = "unsat" then true
      else has_unsat (i + 1)
    in has_unsat 0
  ) lines

let clean_line line = (* here i assumed that z3 outputs as "v x1 x2..." (v-included) or " x1 x2..." (v not included)*)
  let line = String.trim line in (* trim removes spaces (if any at start or at end)*)
  if String.length line > 2 && line.[0] = 'v' && line.[1] = ' ' then
    String.sub line 2 (String.length line - 2) (* if v is included*)
  else if String.length line > 0 && (line.[0] = '-' || (line.[0] >= '0' && line.[0] <= '9')) then
    line (* if line starts with number or minus sign --> v is not included*)
  else
    "" 

let all_numbers = (* got all variable ids with their respective signs in a list "all_numbers". *)
  List.flatten (
    List.map (fun line ->
      List.filter_map int_of_string_opt (String.split_on_char ' ' (clean_line line))
    ) lines
  )

let positive_vars = List.filter (fun x -> x > 0) all_numbers

let max_var = List.fold_left max 0 positive_vars
let n = if max_var <= 819 then 9 else 16 (* Since var_id >4096 exists for n=16 *)

let var_id i j v = n*n*i + n*j + v 

let decode var =
  let v_rem = var mod n in
  let v =
    if n = 9 then (if v_rem = 0 then 9 else v_rem)
    else v_rem
  in
  let temp = (var - v) / n in      (* temp = n*i + j *)
  let j_rem = temp mod n in
  let j = if j_rem = 0 then n else j_rem in
  let i = (temp - j) / n in
  (i, j, v)

let grid = Array.make_matrix n n (-1)

let () =
  let oc = open_out "output.txt" in
  if is_unsat then (
    Printf.fprintf oc "No solution\n";
    close_out oc
  ) else (
    List.iter (fun var ->
      let (i, j, v) = decode var in
      if i >= 1 && i <= n && j >= 1 && j <= n then
        grid.(i-1).(j-1) <- v (* since my indices are 1-based. *)
    ) positive_vars;
    
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let v = grid.(i).(j) in
        if v >= 0 && v <= 9 then Printf.fprintf oc "%d" v
        else if v >= 10 && v <= 15 then Printf.fprintf oc "%c" (Char.chr (Char.code 'A' + v - 10))
        else Printf.fprintf oc "."
      done;
      Printf.fprintf oc "\n"
    done;
    close_out oc
  )