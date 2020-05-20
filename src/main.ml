open Printf
open Command
open Functions
open Mathfunc
open Graphs
open Graphics
open Rsa

exception VariableNotFound
exception MalformedRegression

let print_help () = "List of Commands: 
                'help': list of all commands
                'exit' : exits the program
                'save' file_name.csv : saves file
                'nrows' : number of rows in file
                'ncols' : number of cols in file
                'shape' : number of rows, number of cols in file
                'readr' : reads the row at the given index in file
                'readcol'  index : reads the col at the given index in file
                'delrow'  index : deletes the row at the given index in file
                'delcol'  index : deletes the col at the given index in file
                'insertr' row index : inserts the row at the given index
                'insertc' col index : inserts the col at the given index
                'graph' : allows user to create basic plot with two data columns
                'mean' var_name : mean of data set
                'median' var_name : median of data set
                'mode' var_name : mode of data set
                'variance' var_name: variance of data set
                'stdev' var_name: standard deviation of data set
                'add_data' var_name number: add number to every element in 
                           variable list
                'mult_data' var_name number: multiply every element in variable 
                            list by number
                'z-test' var_name int: z-test on var_name for mean equal to int
                'regress' independent_var dependent_var1 dependent_var2: 
                          regress dependent variables on independent vars
                          can have 1 or 2 dependent vars. Incomplete function
                'gen' var_name int: generates a variable named variable name 
                                    equal to list of elements in int column of 
                                    file
                'print_var' var_name : prints elements in var_name
                'priv_key' e p q : returns the private RSA key for the values e 
                                  (co prime with phi), p (prime), and q (prime) 
                'pub_key' e p q : returns the public RSA key for the values e 
                                  (co prime with phi), p (prime), and q (prime)                                                                                     
                'encrypt' e n : encrypts the dataset. e is the first entry of 
                                the pub_key, n is the second entry
                'decrypt' d n : decrypts the dataset. e is the first entry of the 
                                priv_key, n is the second entry"

let rec find_var var_name var_ls =
  match var_ls with 
  | [] -> raise VariableNotFound
  | (n,d)::t -> if n = var_name then d else find_var var_name t

let rec comm_helper str data var_ls: unit = 
  print_endline "\nPlease enter a command";
  print_string ">>> ";
  try (
    let inp = read_line () |> parse_command in 
    match inp with 
    | Load file -> 
      let conc_str = String.concat "" file in
      let inp_csv = read_csv conc_str in
      Functions.pp_curr_csv inp_csv; comm_helper conc_str inp_csv var_ls
    | Save -> 
      let r_csv = read_back_csv data in 
      print_string "What would you like to call your saved file? ";
      let inp_name = read_line () in
      let fname = Filename.concat (Filename.get_temp_dir_name()) inp_name in
      Csv.save fname r_csv;
      printf "Saved CSV to file %S.\n" fname; comm_helper str data var_ls
    | Graph -> begin Graphs.run data (); comm_helper str data var_ls end
    | Help -> ANSITerminal.(print_string [yellow] (print_help())); 
      comm_helper str data var_ls
    | Exit -> ANSITerminal.(print_string [green] "Thanks for using our DBMS.\n"); 
      exit 0 
    | Nrows -> data |> num_of_rows |> string_of_int |> print_endline; 
      comm_helper str data var_ls
    | Ncols -> data |> num_of_cols |> string_of_int |> print_endline; 
      comm_helper str data var_ls
    | Shape -> let ls = (shape data) 
      in ("rows: "^ string_of_int (List.nth ls 0) ^ " cols: " 
          ^ (string_of_int (List.nth ls 1))) |> print_endline; 
      comm_helper str data var_ls
    | Readrow i ->  print_endline (pp_ls(read_row data i)); 
      comm_helper str data var_ls
    | Readcol i ->  print_endline (pp_ls(read_col data i)); 
      comm_helper str data var_ls
    | Delrow i ->  let d = delete_row data i in Functions.pp_curr_csv d; 
      comm_helper str d var_ls
    | Delcol i ->  let d = delete_col data i in Functions.pp_curr_csv d; 
      comm_helper str d var_ls
    | Insertr (l,i) -> let d =(insert_row data l i) in Functions.pp_curr_csv d; 
      comm_helper str d var_ls
    | Insertc (l,i) -> let d = insert_col data l i in Functions.pp_curr_csv d; 
      comm_helper str d var_ls
    | Gen_var (var_name, index) -> let r = read_col data index 
      in print_endline(pp_ls (r)); 
      comm_helper str data ((var_name,r):: var_ls)
    | Print_chart (var_name, index) -> print_chart var_name 
                                         (find_var var_name var_ls) index; 
      comm_helper str data var_ls
    | Print_var s -> print_endline(pp_ls(find_var s var_ls)); 
      comm_helper str data var_ls
    | Mean var_name -> find_var var_name var_ls |> mean |> string_of_float 
                       |> print_endline; comm_helper str data var_ls
    | Median var_name -> find_var var_name var_ls |> median |> string_of_float 
                         |> print_endline; comm_helper str data var_ls
    | Mode var_name -> find_var var_name var_ls |> mode |> pp_ls |> 
                       print_endline; comm_helper str data var_ls
    | Variance var_name -> find_var var_name var_ls |> variance |> string_of_float 
                           |> print_endline; comm_helper str data var_ls
    | Stdev var_name -> find_var var_name var_ls |> std_dev |> string_of_float 
                        |> print_endline; comm_helper str data var_ls
    | Add_Data (s,i) -> add_data (find_var s var_ls) i |> pp_ls |> print_endline; 
      comm_helper str data var_ls
    | Mult_Data (s,i) -> print_endline(pp_ls (mult_data (find_var s var_ls) i)); 
      comm_helper str data var_ls
    | Regress var_list -> (match var_list with 
        | y::x2::x1::[] -> let y'= (find_var y var_ls) in 
          let x2' = (find_var x2 var_ls) in
          let x1' = (find_var x1 var_ls) in
          form_reg_ls_3 (y,y') (x2, x2') (x1, x1'); comm_helper str data var_ls
        | y::x::[] -> let y'= (find_var y var_ls) in 
          let x' = (find_var x var_ls) in
          form_reg_ls_2 (y,y') (x,x'); comm_helper str data var_ls
        | _ -> (raise MalformedRegression))
    | Pub_K (e,p,q) -> print_endline(let e,n= (pub_key e p q) 
                                     in "Your public key is: " ^ 
                                        (string_of_int e) ^ ", " ^ 
                                        (string_of_int n)); 
      comm_helper str data var_ls
    | Priv_K (e,p,q) -> print_endline(let e,n= (priv_key e p q) in 
                                      "Your private key is: " 
                                      ^ (string_of_int e) ^ ", " 
                                      ^ (string_of_int n)); 
      comm_helper str data var_ls
    | Encrypt (e1, e2) -> let d = encrypt_dat data (e1,e2) 
      in Functions.pp_curr_csv d; 
      comm_helper str d var_ls
    | Decrypt (e1, e2) -> let d = decrypt_dat data (e1,e2) 
      in Functions.pp_curr_csv d; 
      comm_helper str d var_ls
  )
  with | Command.Invalid_Command s -> 
    print_endline (s^(" Type help for list of commands")); 
    comm_helper str data var_ls
       | InvalidIndex -> print_endline ("Invalid Index."); 
         comm_helper str data var_ls
       | InvalidFile s -> print_endline s; comm_helper str data var_ls
       | VariableNotFound -> print_endline ("variable not found"); 
         comm_helper str data var_ls
       | EmptyList -> print_endline ("Variable is empty"); 
         comm_helper str data var_ls
       | Incorrect_Input s -> print_endline s; comm_helper str data var_ls
       | DNE -> print_endline "the mod inverse did not exist. try again!"; 
         comm_helper str data var_ls

let csv_import csv_str = 
  List.map (fun name -> name, Csv.load name)
    [csv_str] 

let rec get_file () = 
  print_endline "Please enter the name of the CSV file you want to load.\n";
  print_string  "> ";
  try (
    match read_line () with 
    | "exit" -> ANSITerminal.(print_string [green] "Thanks for using our DBMS.\n");
      exit 0 
    | exception End_of_file -> ()
    | file_name -> List.iter (
        fun (name, csv) ->
          printf "---%s----------------------------------------\n" name;
          Csv.print_readable csv
      ) (csv_import file_name); comm_helper file_name (read_csv file_name) 
        ([(file_name, (gen_data_ls (read_csv file_name)))])
  ) 
  with |Sys_error s -> print_endline s; get_file ()

let rec main () = 
  ANSITerminal.(print_string [green] 
                  "\n\nThis is an OCaml Database Management System.\n");
  get_file()


let () = main ()
