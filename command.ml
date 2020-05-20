
exception Invalid_Command of string
exception InvalidIndex 
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
  | Readrow of int
  | Readcol of int
  | Delrow of  int  
  | Delcol of int 
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

(* [parse_s s] is a helper function to parse a string of a float list (and here,
   returns another float)
   Requires: [s] a string 
   Raises: [Invalid_Index] if an out of bounds index is given
   Raises: [Malformed_Command] if the command does not parse correctly*)
let parse_s s =
  let ls = (String.split_on_char '[' (String.trim s)) in
  match ls with 
  | h::t::[] ->( match (String.split_on_char ']' (String.trim t)) with 
      | [] -> raise InvalidIndex
      | h::t::[] -> h 
      | _ -> raise Malformed)
  | h::h2::t -> raise InvalidIndex
  | _ -> raise Malformed

(** [float_ls s] returns string [s] as a float list
    Requires: [s] is a string*)
let float_ls s =
  (String.split_on_char ';' (parse_s s)) |> List.filter (fun x -> not (x = "")) 
  |> List.map float_of_string 

(** [parse_command input] returns a command of type basic_command according
    to user's input [input]
    Requires: [input] is a string
    Raises: [Invalid_Command] if user enters an invalid command*)
let parse_command input =
  input
  |> String.trim
  |> String.lowercase_ascii 
  |> (fun x -> if x = ""
       then raise (Invalid_Command "Please type a command.") 
       else x)
  |> String.split_on_char ' '
  |> List.map String.trim
  |> List.filter (fun x -> not (x = ""))
  |> (fun str -> match str with
      | cmd::rest -> (
          match cmd with
          | "load" -> Load rest
          | "graph" -> Graph
          | "save" -> Save 
          | "exit" -> Exit
          | "help" -> Help 
          | "nrows" -> Nrows 
          | "ncols" -> Ncols 
          | "shape" -> Shape 
          | "readr" -> (match rest with 
              | h::[] -> (let h' = (int_of_string h) in Readrow h')
              |  _ -> raise (Invalid_Command "This command is invalid.")) 
          | "readcol" -> (match rest with 
              | h::[] -> (let h' = (int_of_string h) in Readcol h')
              |  _ -> raise (Invalid_Command "This command is invalid.")) 
          | "delrow" -> (match rest with 
              | h::[] -> (let h' = (int_of_string h) in Delrow h')
              |  _ -> raise (Invalid_Command "This command is invalid.")) 
          | "delcol" -> (match rest with 
              | h::[] -> (let h' = (int_of_string h) in Delcol h')
              |  _ -> raise (Invalid_Command "This command is invalid.")) 
          | "insertr" -> (match rest with 
              | l::i::[] -> (let i2 = (int_of_string i) in let l2 = (float_ls l) 
                             in Insertr (l2,i2))
              |  _ -> raise (Invalid_Command "This command is invalid!")) 
          | "insertc" -> (match rest with 
              | l::i::[] -> (let i2 = (int_of_string i) in let l2 = (float_ls l) 
                             in Insertc (l2,i2))
              |  _ -> raise (Invalid_Command "This command is invalid."))  
          | "gen" -> (match rest with 
              | s::i::[] -> (let i2 = (int_of_string i) in Gen_var (s,i2))
              | _ -> raise (Invalid_Command "Malformed mean"))
          | "mean" -> (match rest with 
              | h::[] -> Mean h 
              | _ -> raise (Invalid_Command "Malformed mean"))
          | "median" -> (match rest with 
              | h::[] -> Median h 
              | _ -> raise (Invalid_Command "Malformed median"))
          | "mode" -> (match rest with 
              | h::[] -> Mode h 
              | _ -> raise (Invalid_Command "Malformed mode"))
          | "variance" -> (match rest with 
              | h::[] -> Variance h 
              | _ -> raise (Invalid_Command "Malformed variance"))
          | "stdev" -> (match rest with 
              | h::[] -> Stdev h 
              | _ -> raise (Invalid_Command "Malformed stdev"))
          | "add_data"-> (match rest with 
              | s::i::[] -> let i2 = float_of_string i in Add_Data (s,i2)
              | _ -> raise (Invalid_Command "Malformed add_data"))
          | "mult_data"-> (match rest with 
              | s::i::[] -> let i2 = float_of_string i in Mult_Data (s,i2)   
              | _ -> raise (Invalid_Command "Malformed mult_data"))
          | "z-test"-> (match rest with 
              | s::i::[] -> let i2 = float_of_string i in Print_chart (s,i2)
              | _ -> raise (Invalid_Command "Malformed z-test_data"))
          | "regress" -> Regress rest
          | "print_var" -> (match rest with 
              | h::[] -> Print_var h 
              | _ -> raise (Invalid_Command "Malformed print_var"))
          | "pub_key" -> (match rest with 
              | e::p::q::[] -> 
                Pub_K (int_of_string e, int_of_string p, int_of_string q)
              | _ -> raise (Invalid_Command "Malformed pub_key"))
          | "priv_key" -> (match rest with 
              | e::p::q::[] -> 
                Priv_K (int_of_string e, int_of_string p, int_of_string q)
              | _ -> raise (Invalid_Command "Malformed priv_key"))
          | "encrypt" -> (match rest with 
              | e1::e2::[] -> Encrypt (int_of_string e1, int_of_string e2)
              | _ -> raise (Invalid_Command "Malformed encrypt"))
          | "decrypt" -> (match rest with 
              | e1::e2::[] -> Decrypt (int_of_string e1, int_of_string e2)
              | _ -> raise (Invalid_Command "Malformed decrypt"))
          | _ -> raise (Invalid_Command "That command is invalid.")
        )
      |  _ -> raise (Invalid_Command "This command is invalid.")
    )




