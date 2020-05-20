type t 
type index 
type row
type col 

(* raised when an element cannot be found in the database *)
exception InvalidElmt of float

(* raised when an invalid string is given for a file name *)
exception InvalidFile of string 

(* raised when an invalid index is given to access a row/col *)
exception InvalidIndex 

(* raised when an invalid t element is created/used *)
exception InvalidCSV

(* [read_csv name] gives the csv file translated to type t from the file name
   Requires: [name] is the name of the file 
   Raises: [InvalidFile name] if the name of the file is invalid
   Raises [InvalidCSV] if the t doesn't satisfy the RI*)
val read_csv: string -> t 

(* [read_back_csv name] gives a csv file translated from type t to Csv.t
   Requires: [data] is a valid t that satisfies rep_ok 
   Raises [InvalidCSV] if the t doesn't satisfy the RI *)
val read_back_csv: t -> string list list

(* [num_of_rows data] returns the number of rows in the database
   Requires: [data] is a valid t that satisfies rep_ok *)
val num_of_rows: t -> int 

(* [num_of_cols data] returns the number of cols in the database 
   Requires: [data] is a valid t that satisfies rep_ok*)
val num_of_cols: t -> int

(* [shape data] returns a tuple of the number of rows and columns in the 
   database 
   Requires: [data] is a valid t that satisfies rep_ok*)
val shape: t -> int list

(* [read_row data index] returns a given row that corresponds with the 
   given index
   Requires: [data] is a valid t that satisfies rep_ok
   Requires: [index] is any integer
   Raises: [InvalidIndex] if an out of bounds index is given *)
val read_row: t -> int -> float list 

(* [insert_row data row index] returns a new databse with the given row inserted
   at the correct index 
   Requires: [data] is a valid t that satisfies rep_ok
   Requires: [row] is a elmt list of the correct size (# of elmts equals the
   number of columns)
   Requires: [index] is any integer 
   Raises: [InvalidIndex] if the index is out of bounds (must be between 0 and 
   num_of_rows (inclusive) ) 
   Raises [InvalidCSV] if the t doesn't satisfy the RI*)
val insert_row: t -> float list -> int -> t

(* [delete_row data index] returns a new database with a row removed that 
   corresponds to the index given
   Requires: [data] is a valid t that satisfies rep_ok
   Requires: [index] is any integer
   Raises: [InvalidIndex] if an out of bounds index is given *)
val delete_row: t -> int -> t

(* [read_col data index] returns a given col that corresponds with the 
   given index
   Requires: [data] is a valid t that satisfies rep_ok
   Requires: [index] is any integer
   Raises: [InvalidIndex] if an out of bounds index is given *)
val read_col: t -> int -> float list 

(* [insert_col data col index] returns a new databse with the given col inserted
   at the correct index 
   Requires: [data] is a valid t that satisfies rep_ok
   Requires: [col] is a elmt list of the correct size (# of elmts equals the
   number of rows)
   Requires: [index] is any integer 
   Raises: [InvalidIndex] if an out of bounds index is given 
   Raises [InvalidCSV] if the t doesn't satisfy the RI *)
val insert_col: t -> float list -> int -> t

(* [delete_col data index] returns a new database with a col removed that 
   corresponds to the index given
   Requires: [data] is a valid t that satisfies rep_ok
   Requires: [index] is any integer 
   Raises: [InvalidIndex] if an out of bounds index is given *)
val delete_col: t -> int -> t

(* [pp_curr_csv data] prints the current csv
   Requires: [data] valid t*)
val pp_curr_csv: t -> unit

(* [gen_data_ls t] returns a flattened t element
   Requires: [array] the data to flatten *)
val gen_data_ls: t -> float list