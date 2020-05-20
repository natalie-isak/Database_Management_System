(* raised when an invalid command is given *)
exception Invalid_Command of string

(* raised when an invalid index is referenced for a csv file *)
exception InvalidIndex 

(* raised when a malformed command is given (parsing failes) *)
exception Malformed

type obj_phrase = string list

type basic_command = 
  | Load of obj_phrase 
  | Graph
  | Save 
  | Exit 
  | Help 
  | Nrows 
  | Ncols 
  | Shape 
  | Readrow of (int) 
  | Readcol of (int) 
  | Delrow of (int)  
  | Delcol of (int)
  | Insertr of (float list) * int
  | Insertc of (float list) * int
  | Gen_var of (string * int)
  | Mean of string
  | Median of string
  | Mode of string
  | Variance of string
  | Stdev of string
  | Add_Data of (string * float)
  | Mult_Data of (string * float)                                          
  | Print_var of string 
  | Print_chart of (string * float)
  | Regress of string list
  | Pub_K of int * int * int
  | Priv_K of int * int * int
  | Encrypt of int * int 
  | Decrypt of int * int 

(* [float_ls s] converts the string into a float list
   Requires: [s] a string 
   Raises: [Invalid_Index] if an out of bounds index is given
   Raises: [Malformed_Command] if the command does not parse correctly*)
val float_ls: string -> float list

(* [parse_command s] parses the command typed into repl
   Requires: [s] a string 
   Raises: [Invalid_Command] if the command does not meet the requirements set 
   out in the "help" command*)
val parse_command: string -> basic_command