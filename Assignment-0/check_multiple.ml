
let sat_lines = In_channel.with_open_text Sys.argv.(1) In_channel.input_lines
let cnf_lines = In_channel.with_open_text Sys.argv.(2) In_channel.input_lines
(* first argument is sat_output.txt and 2nd is problem.cnf --> checkout Makefile*)

let all_numbers =
  List.flatten (
    List.map (fun line ->
      List.filter_map int_of_string_opt (String.split_on_char ' ' line) (* int_of_string_opt returns None for non-integer chars. *)
    ) sat_lines
  )

let pos_vars = List.filter (fun x -> x > 0) all_numbers (* extracting out all positive vars. *)

let has_soln = List.length pos_vars > 0 (* if no solution then empty list. *)

let blocking_clause = List.map (fun x -> -x) pos_vars (* adding blocking clause prevents z3 to search the prev soln. *)

let parse_header line = (* since my header line is " p cnf &d %d", so those int values will be taken into the list.filtermap *)
  let parts = String.split_on_char ' ' line in
  let nums = List.filter_map int_of_string_opt parts in
  match nums with
  | [v; c] -> (v, c)
  | _ -> (0, 0)

let (num_vars, num_clauses) = 
  let header_line = List.find (fun l -> String.length l > 0 && l.[0] = 'p') cnf_lines in
  parse_header header_line

let () = 
  if not has_soln then (
    Printf.printf "p cnf 1 2\n";
    Printf.printf "1 0\n";
    Printf.printf "-1 0\n" (* having both +1 and -1 ensures for no soln. *)
  ) else (
    Printf.printf "p cnf %d %d\n" num_vars (num_clauses + 1);
    List.iter (fun line ->
      if String.length line > 0 && line.[0] <> 'p' && line.[0] <> 'c' then
        Printf.printf "%s\n" line
    ) cnf_lines;
    List.iter (fun lit -> Printf.printf "%d " lit) blocking_clause;
    Printf.printf "0\n"
  )