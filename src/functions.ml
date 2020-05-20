type t = float list list 
type index = int  
type row = float list 
type col = float list 

exception InvalidIndex 
exception InvalidElmt of float
exception InvalidFile of string
exception InvalidCSV

(* [get_len dat acc] is a helper function which returns a list of the lengths of 
   rows. 
   Requires: [dat] any int list list 
   Requires: [acc] a list to accumulate with   *)
let rec get_len dat acc = 
  match dat with 
  | [] -> []
  | h::t -> (List.length h) :: get_len t acc

(* [rep_ok data] returns t if t satisfies the preconditions: t must have an 
   equal number of all rows and an equal number of all columns. 
   Requires: [data] any int list list 
   Raises [InvalidCSV] if the t doesn't satisfy the RI *)
let rep_ok data = 
  let len = get_len data [] in 
  let rec help_2 len = 
    match len with 
    | [] -> data 
    | x::[] -> data
    | x::y::z -> if x <> y then raise InvalidCSV else help_2 (y::z) 
  in help_2 len 

let read_csv str = 
  try 
    let rec help x = 
      match x with 
      | [] -> [] 
      | h::t -> let rec row_help m = 
                  match m with 
                  | w::z -> (float_of_string w)::row_help z
                  | _ -> [] 
        in (row_help h) :: help t 
    in str |> Csv.load |> help |> rep_ok
  with _ -> raise (InvalidFile
                     "Invalid string. Try inputting a different file name.")

let read_back_csv i_list = 
  let rec help x = 
    match x with 
    | [] -> [] 
    | h::t -> let rec row_help m = 
                match m with 
                | w::z -> (string_of_float w)::row_help z
                | _ -> [] 
      in (row_help h) :: help t 
  in i_list |> rep_ok |> help

let num_of_rows data = 
  let rec helper data acc = 
    match data with 
    | [] -> acc
    | h::t -> helper t (acc+1) 
  in helper data 0

let num_of_cols data =
  match data with 
  | [] -> 0
  | h::t -> List.length h 

let shape data =
  [num_of_rows data; num_of_cols data]

(* [get_row row] returns the row index of a given row
   Requires: [data] a valid elmt list list that satisfies rep_ok
   Requires: [acc] an integer which counts the current num of rows
   Requires: [index] an integer of the desired index of the row  
   Raises [InvalidIndex] if the given row is empty *)
let rec get_row data acc index = 
  match data with 
  | [] -> raise InvalidIndex
  | h::t -> if acc = index then h else get_row t (acc+1) index 

let rec read_row data index =  
  match data with 
  | [] -> raise InvalidIndex
  | h::t -> get_row data 0 index 

(* [aft_or_at_index data index acc] returns the elmts of a list before a given 
   index
   Requires: [data] an list 
   Requires: [index] an integer which indicates which element in the dataset
   Requires: [acc] a counter variable to keep track of the current index
   Raises: [Invalid Index] if index is below 0 or above the num of rows*)
let rec aft_or_at_index data index acc = 
  match data with 
  | [] -> if num_of_rows data < index || index < 0 then raise InvalidIndex 
    else [] 
  | h::t -> if index = 0 then data 
    else if index = acc then data
    else aft_or_at_index t index (acc+1)

let insert_row data input index = 
  try 
    if index = 0 then input::aft_or_at_index data index 0 |> rep_ok 
    else if index = num_of_rows data then data @ [input] |> rep_ok 
    else let top = aft_or_at_index (List.rev data) ((List.length data) - index) 
             0 |> List.rev in top @ (input :: aft_or_at_index data index 0) 
                              |> rep_ok 
  with InvalidIndex -> raise InvalidIndex

let rec delete_row data index = 
  match data with 
  | [] -> raise InvalidIndex
  | h::t -> if num_of_rows data < index || index < 0 then raise InvalidIndex 
    else if index = 0 then t
    else if index = num_of_rows data -1 then delete_row (List.rev data) 0 
                                             |> List.rev 
    else let top = aft_or_at_index (List.rev data) ((List.length data) - (index)) 
             0 |> List.rev in top @ (aft_or_at_index data (index+1) 0) 

(* [get_col col] returns the col index of a given elmt
   Requires: [row] an elmt list (essentially a row), a subsection of a valid 
   data variable 
   Requires: [index] an integer which indicates which element in the row
   Requires: [acc] a counter variable to keep track of the current index
   Raises [InvalidIndex] if the given row is empty *)
let rec get_col_elmt row index acc = 
  match row with 
  | [] -> raise InvalidIndex
  | h::t -> if acc = index then h else get_col_elmt t index (acc+1)

let read_col data index = 
  if num_of_cols data < index || index < 0 then raise InvalidIndex else 
    let rec help data index = 
      match data with 
      | [] -> [] 
      | h::t -> (get_col_elmt h index 0) :: (help t index) 
    in help data index

let rec insert_col data input index = 
  match data, input with 
  | h::t,x::y -> 
    if index = 0 then [x::h] @ insert_col t y index |> rep_ok 
    else if index = num_of_cols data then [h@[x]] @ insert_col t y index |> rep_ok 
    else [(List.rev (aft_or_at_index (List.rev h) ((List.length h) - index) 0)) 
          @ (x :: aft_or_at_index h index 0)] @ insert_col t y index |> rep_ok 
  | [], x::y -> if index = 0 then [x::[]] @ insert_col data y index 
    else raise InvalidIndex
  | _ , [] -> data

let delete_col data index = 
  if data = [] then raise InvalidIndex else 
    let rec help data index = 
      match data with 
      | h::t -> 
        if index = 0 then match h with 
          | [] -> []
          | x::y -> [y] @ help t index 
        else if index+1 = num_of_cols data then match (List.rev h) with 
          | [] -> []
          | x::y -> [List.rev y] @ help t index 
        else 
          [(List.rev (aft_or_at_index (List.rev h) ((List.length h) - index) 0)) 
           @ (aft_or_at_index h (index+1) 0)] @ help t index
      | _-> []
    in help data index

(* [gen_data_ls t] returns a flattened t element
   Requires: [array] the data to flatten *)
let gen_data_ls array = List.flatten array

(* [pp_curr_csv data] prints the current csv
   Requires: [data] valid t*)
let pp_curr_csv data = let d = read_back_csv data in
  Csv.print_readable d