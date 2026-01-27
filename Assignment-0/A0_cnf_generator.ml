let lines = In_channel.with_open_text "input.txt" In_channel.input_lines
let grid = lines  
let n = List.length grid

let var_id i j v = (* gives 1 to 9 for n=9 and 0 to 15 for n=16*)
  (* i,j are 1-based indices represent location in grid and v is the value in that location*)
  n*n*i +n*j + v;;


let char_to_value c =
  if c >= '1' && c <= '9' then Some (Char.code c - Char.code '0')
  else if c >= 'A' && c <= 'F' then Some (Char.code c - Char.code 'A' + 10)
  else if c='0' && n=16 then Some 0
  else None  (* for '.' char *)

let input_clauses = (* 1. constraint due to input sudoku*)
  List.flatten (
    List.map (fun i ->
      let row = List.nth grid i in
      List.filter_map (fun j ->
        match char_to_value row.[j] with
        | Some v -> Some [var_id (i + 1) (j + 1) v]
        | None -> None
      ) (List.init n (fun x -> x))
    ) (List.init n (fun x -> x))
  )

let rec cell_at_least_one_help i j k n l=
    match (k, n) with
    | (0, 9) -> l
    | (-1, 16) ->l
    | (_, _) -> var_id i j k :: (cell_at_least_one_help i j (k-1) n l)
      

let doglapan = (* got forced to use it in my Helper recursions due to assignment's doglapan requirement*)
  if n=9 then 9 (* doglapan: starting value for recursion 9x9 uses values 1-9, so start at 9 and 16x16 uses values 0-15, so start at 15 *)
  else if n=16 then 15
  else 0 (* default value*)

let cell_at_least_one = (* 2. constraint due to atleast one number should occupy every cell*)
  List.flatten (
    List.map (fun i ->
      List.map(fun j-> cell_at_least_one_help i j doglapan n [])
       (List.init n (fun x -> x+1))
    ) (List.init n (fun x -> x+1))
  )
  
let rec cell_at_most_one_help i j k1 k2 n l = 
  let min_val = if n = 9 then 1 else 0 in
  if k1 < min_val then l
  else if k2 < min_val then cell_at_most_one_help i j (k1-1) (k1-2) n l
  else [-(var_id i j k1); -(var_id i j k2)] :: (cell_at_most_one_help i j k1 (k2-1) n l)


let cell_at_most_one = (* 3. constraint due to atmost one number should occupy every cell*)
  List.flatten(
    List.flatten (
      List.map (fun i ->
        List.map (fun j -> cell_at_most_one_help i j doglapan (doglapan-1) n [])
        (List.init n (fun x -> x+1))
      ) (List.init n (fun x -> x+1))
    )
  )
let rec row_at_least_one_help i j v n l = 
  match (j, n) with
  | (0, 9) -> l
  | (-1, 16) -> l
  | (_, _) -> var_id i j v :: (row_at_least_one_help i (j-1) v n l)


let dogla_value_list = (* due to assignment's doglapan*)
  if n = 9 then List.init 9 (fun x -> x + 1)      (* [1;2;...;9] *)
  else List.init 16 (fun x -> x)                   (* [0;1;...;15] *)

let row_at_least_one = (* 4. constraint due to every num should present atleast once in every row*)
  List.flatten (
    List.map (fun i ->
      List.map (fun v -> row_at_least_one_help i n v n [])
      dogla_value_list
    ) (List.init n (fun x -> x + 1))
  )


let rec row_at_most_one_help i j1 j2 v n l = 
  if j1 < 1 then l
  else if j2 < 1 then row_at_most_one_help i (j1-1) (j1-2) v n l
  else [-(var_id i j1 v); -(var_id i j2 v)] :: (row_at_most_one_help i j1 (j2-1) v n l)


let row_at_most_one = (*5. constraint due to every num should present atmost once in every row*)
  List.flatten (
    List.flatten (
      List.map (fun i ->
        List.map (fun v -> row_at_most_one_help i n (n-1) v n [])
        dogla_value_list
      ) (List.init n (fun x -> x + 1))
    )
  )


let rec col_at_least_one_help i j v n l = 
  match (i, n) with
  | (0, 9) -> l
  | (-1, 16) -> l
  | (_, _) -> var_id i j v :: (col_at_least_one_help (i-1) j v n l)


let col_at_least_one = (* 6. constraint due to each value appears at least once in each column *)
  List.flatten (
    List.map (fun j ->
      List.map (fun v -> col_at_least_one_help n j v n [])
      dogla_value_list
    ) (List.init n (fun x -> x + 1))
  )


let rec col_at_most_one_help i1 i2 j v n l = 
  if i1 < 1 then l
  else if i2 < 1 then col_at_most_one_help (i1-1) (i1-2) j v n l
  else [-(var_id i1 j v); -(var_id i2 j v)] :: (col_at_most_one_help i1 (i2-1) j v n l)


let col_at_most_one = (* 7.constraint due to each value appears at most once in each column*)
  List.flatten (
    List.map (fun j ->
      List.flatten (
        List.map (fun v -> col_at_most_one_help n (n-1) j v n [])
        dogla_value_list
      )
    ) (List.init n (fun x -> x + 1))
  )


let sq_n = int_of_float (sqrt (float_of_int n))

let block_cells bi bj =
  List.flatten (
    List.init sq_n (fun di ->
      List.init sq_n (fun dj -> (bi * sq_n + di + 1, bj * sq_n + dj + 1))
    )
  )

let pairs cells =
  List.flatten (
    List.map (fun c1 ->
      List.filter_map (fun c2 -> if c1 > c2 then Some (c1, c2) else None) cells
    ) cells
  )

let block_at_least_one = (* 8. constraint due to atleast once in each block*)
  List.flatten (
    List.init sq_n (fun bi ->
      List.flatten (
        List.init sq_n (fun bj ->
          List.map (fun v ->
            List.map (fun (i, j) -> var_id i j v) (block_cells bi bj)
          ) dogla_value_list
        )
      )
    )
  )

let block_at_most_one = (* 9. constraint due to atmost once in each block*)
  List.flatten (
    List.init sq_n (fun bi ->
      List.flatten (
        List.init sq_n (fun bj ->
          List.flatten (
            List.map (fun v ->
              List.map (fun ((i1, j1), (i2, j2)) ->
                [-(var_id i1 j1 v); -(var_id i2 j2 v)]
              ) (pairs (block_cells bi bj))
            ) dogla_value_list
          )
        )
      )
    )
  )
  

let print_clause clause =
  List.iter (fun lit -> print_int lit; print_string " ") clause;
  print_string "0\n"

let all_clauses =
  input_clauses @
  cell_at_least_one @
  cell_at_most_one @
  row_at_least_one @
  row_at_most_one @
  col_at_least_one @
  col_at_most_one @
  block_at_least_one @
  block_at_most_one

let num_vars = n * n * n + n * n + doglapan  (* max var_id = n*n*n + n*n + max_value *)
let num_clauses = List.length all_clauses

let () =
  Printf.printf "p cnf %d %d\n" num_vars num_clauses;
  List.iter print_clause all_clauses




  