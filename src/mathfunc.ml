open Functions
exception EmptyList

(*[ls_length data] returns the length of [data]
  Requires: [data] is a float list 
  Raises [EmptyList] if length = 0*)
let ls_length data = match List.length data with 
  | 0 -> raise EmptyList 
  | i -> i

(** [to_string ls acc] returns the list seperated by ;
    Requires: [ls] is a float list
    Requires: [acc] is a string list*)
let rec to_string ls acc =
  match ls with 
  | [] -> acc 
  | h::t -> to_string t ((string_of_float h) ^ "; "^acc)

let pp_ls ls = "["^(to_string ls "")^"]"

(*[to_string_c ls acc] returns the 95%CI list as a string
  Requires: [ls] is a float list 
  Requires: [acc] is a string list *)
let to_string_c ls acc =
  match ls with 
  | h::t::[] -> "["^(string_of_float h)^", "^(string_of_float t)^"]"
  | _ -> acc 

(*[pp_conf ls] returns the 95%CI list as a string
  Requires: [ls] is a float list*)
let pp_conf ls = to_string_c ls ""

(** [mult_e x y acc] returns acc as a list of the corresponding elements
    in x and y multiplied together
    Requires: [x] is a float list 
    Requires: [y] is a float list 
    Requires [acc] is a float list*)
let rec mult_e x y acc=
  match x,y with 
  | h::t,h'::t' -> mult_e t t' ((h*.h')::acc)
  | _-> acc

let round_float f = let f2 = Float.trunc (f *. 100000.) in f2/. 100000.

(** [sum_e data] is the sum of the elements in data
    Requires: [data] is a float list*)
let sum_e data = List.fold_right (Float.add) data 0.

let mean data = Float.div (sum_e data)  (float_of_int (ls_length data))

(** [square_e data] is [data] with all elements squared
    Requires: [data] is a float list*)
let square_e data = List.map (fun x -> (Float.pow x 2.)) data 

(** [sum_xy x y] is list with corresponding elemnts of x an y multiplied
    Requires: [x] is a float list 
    Requires: [y] is a float list*)
let sum_xy x y = sum_e(mult_e x y [])

(** [sum_e2 x] is sum of [x] with each element squared
    Requires: [x] is a float list*)
let sum_e2 x = sum_e (square_e x)

(** [n x] returns float of length of [x]
    Requires: [x] is a float list*)
let n x = float_of_int (List.length x)

(** [numerator_3 x2 x1 y] is the numerator of 3 var regression coef.
    Requires: [x2] is a float list 
    Requires: [x1] is a float list 
    Requires: [y] is a float list*)
let numerator_3 x2 x1 y = ((sum_e2 x2)*.(sum_xy x1 y))-.((sum_xy x1 x2) 
                                                         *. sum_xy x2 y)

(** [denominator_3 x2 x1] is the denominator of a 3 var regression coef
    Requires: [x2] is a float list 
    Requires: [x1] is a float list*)
let denominator_3 x2 x1 = ((sum_e2 x1)*.(sum_e2 x2))-.
                          (Float.pow (sum_xy x1 x2) 2.)

(** [b1_3 x2 x1 y] is the coef for b1 in 3 var regression
    Requires: [x2] is a float list 
    Requires: [x1] is a float list
    Requires: [y] is a float list*)
let b1_3 x2 x1 y= (numerator_3 x2 x1 y)/.(denominator_3 x2 x1)

(** [b2_3 x2 x1 y] is the coef for b2 in 3 var regression
    Requires: [x2] is a float list 
    Requires: [x1] is a float list
    Requires: [y] is a float list*)
let b2_3 x2 x1 y= (numerator_3 x1 x2 y)/.(denominator_3 x1 x2)

(** [a_3 x2 x1 y] is the coef for a in 3 var regression
    Requires: [x2] is a float list 
    Requires: [x1] is a float list
    Requires: [y] is a float list*)
let a_3 x2 x1 y = (mean y) -. ((b1_3 x2 x1 y)*.(mean x1))-.((b2_3 x2 x1 y)
                                                            *.(mean x2))

(** [numerator_a_2 x y] is the numerator for a in a 2 var regression
    Requires: [x] is a float list 
    Requires: [y] is a float list*)
let numerator_a_2 x y = ((sum_e y)*.(sum_e2 x))-.((sum_e x)*.(sum_xy x y))

(** [numerator_b_2 x y] is the numerator for b in 2 var regression
    Requires: [x] is a float list 
    Requires: [y] is a float list*)
let numerator_b_2 x y = ((n x)*.(sum_xy x y))-.((sum_e x)*.(sum_e y))

(** [denominator_2 x y] is the denominator for a and b in 2 var regression
    Requires: [x] is a float list 
    Requires: [y] is a float list*)
let denominator_2 x y = ((n x)*.sum_e2 x)-.(Float.pow (sum_e x) 2.)

(** [a_2 x y] is the coef for a in 2 var regression
    Requires: [x] is a float list 
    Requires: [y] is a float list*)
let a_2 x y = (numerator_a_2 x y)/.(denominator_2 x y)

(** [b_2 x y] is the coef for b in 2 var regression
    Requires: [x] is a float list 
    Requires: [y] is a float list*)
let b_2 x y = (numerator_b_2 x y)/.(denominator_2 x y)

(** [map_xi_xbar_sq xi x] is the list of the square of the observes value
    minus the expected value
    Requires: [x] is a float list 
    Requires: [xi] is a float*)
let map_xi_xbar_sq xi x = Float.pow (xi -. (mean x)) 2.

(** [ssx x] is the sum of the list for which each element is the square of the 
    observed value minus the mean of the set
    Requires: [x] is a float list*)
let ssx x = 
  let map_ls = List.map (fun xi -> (map_xi_xbar_sq xi x)) x in 
  (List.fold_right (Float.add) map_ls 0.)/. 2.

(** [exp_obs_ls y x acc a b] is the list of the expected minus the observed 
    values for the dependent variable
    [y] squared for the two-var regression
    Requires: [y] is a float list 
    Requires: [x] is a float list 
    Requires: [acc] is a float list
    Requires: [a] is a flaot 
    Requires: [b] is a float
*)
let rec exp_obs_ls y x acc a b =
  match y, x with 
  | [], [] -> acc
  | yi::t, xi::t'-> 
    exp_obs_ls t t'((Float.pow ((a +. (b *. xi))-. yi) 2.)::acc) a b
  |_,_ -> acc

(** [rmse y x ] is RMSE for the 2 var regression
    Requires: [y] is a float list 
    Requires: [x] is a float list*)
let rmse y x = 
  (List.fold_right (Float.add) (exp_obs_ls y x [] (a_2 x y) (b_2 x y)) 0.)
  /.(float_of_int (ls_length x)) |> Float.pow 0.5

(** [a2_std_err y x] is the standard error for a in the 2 var regression
    Raises [EmptyList] if [x] is empty
    Requires: [y] is a float list 
    Requires: [x] is a float list*)
let a2_std_err y x= (rmse y x*.sum_e2 x/.((float_of_int (ls_length x)*.ssx x)
                                          |> Float.pow 0.5))|> round_float

(** [b2_std_err y x] is the standard error for b in the 2 var regression
    Requires: [y] is a float list
    Requires: [x] is a float list*)
let b2_std_err y x = (rmse y x/.Float.pow (ssx x) 0.5)|> round_float

let median d = let data = (List.sort compare d) in 
  let index = (ls_length data / 2) in        
  if (List.length data) mod 2 = 1 then List.nth data index else 
    (List.nth data (index-1) +. (List.nth data (index)))/.2.

(**[acc_dict h top bottom] returns dictionary [top] updated with the number of 
   occurences of [h] in dictionary [bottom]
   Requires: [h] is a float
   Requires: [top] is a (float*int) list 
   Requires: [bottom] is a (float*int) list *)
let rec acc_dict h top bottom =
  match bottom with 
  | (n,o)::d -> if n = h then top@(n,o+1)::d else acc_dict h ((n,o)::top) d
  | [] -> (h,1)::top

(** [num_dict data dict] returns dictionary [dict] of [data] where keys are the 
    elements in [data] and values are the number of occurances of the value
    in [dict]
    Requires: [data] is a float list 
    Requires: [dict] is a (float*int) list*)
let rec num_dict data dict = 
  match data with 
  | h::t -> num_dict t (acc_dict h [] dict)
  | []-> dict 

(**[max_elt dict acc num] returns the mode of [dict]
   Requires: [num] is a int list
   Requires: [acc] is a float list 
   Requires: [dict] is a (float*int) list*)
let rec max_elt dict acc num= 
  match dict with 
  | (n,o)::d -> if o = num then max_elt d (n::acc) num 
    else if o > num then max_elt d ([n]) o else max_elt d acc num
  | [] -> acc

let mode data = let dict = (num_dict data []) in max_elt dict [] 0                       

let variance data = 
  let dmean = mean data in
  ((List.map (fun x -> (Float.pow (x-.dmean) 2.)) data)|> sum_e)
  /.float_of_int(ls_length data)

let std_dev data= Float.pow (variance data) 0.5

let add_data data num= List.map (fun x -> x+.num) data

let mult_data data num= List.map (fun x -> x*.num) data

(**[test_stat data value] returns the z-stat for the hypothesis 
   test that mean of [data] = [num] 
   Requires: [data] is a float list
   Requires: [value] is a float
   Raises [EmptyList] if [data] is empty*)
let test_stat data value = 
  (mean data-.value)/.(std_dev data/.((float_of_int(ls_length data))
                                      |>Float.pow 0.5))



(** [confidence_int data value] is the confidence interval for [data]
    Requires: [data] is a float list
    Requires: [num] is a float*)
let confidence_int data value = 
  let term = 1.96 *. std_dev data/.Float.pow (float_of_int(ls_length data)) 0.5  
  in let mean_d = mean data in
  [round_float((mean_d -. term)); round_float((mean_d +. term))]

(** [char c num acc] prints a character [c] [num] times
    Requires: [c] is a string
    Requires: [num] is an int
    Requires: [acc] is a string*)
let rec print_char c num acc= 
  if num = 0 then (print_endline acc) else print_char c (num-1) (c^acc)

(** [print_layout word_ls number acc s] returns a string of length 
    [number] for the words in the [word_ls] dict at their 
    respective locations int [word_ls]
    Requires: [word_ls] is a (string*int) list
    Requires: [number] is an int
    Requires: [acc] is a string
    Requires: [s] is a string *)
let rec print_layout word_ls number acc s=
  match word_ls, number with 
  | _, 0 -> print_endline acc
  | (w,i)::t, num -> if num =i then 
      print_layout t (num - (String.length w)) (acc^w) s 
    else print_layout word_ls (num-1) (acc^s) s
  | _, num -> print_layout word_ls (num-1) (acc^s) s

(** [conf_index_finder s_len ci_len] is the index to print 
    "[95% Conf. Interval]" in [print_layout word_ls number acc s]
    Requires: [s_len] is an int
    Requires: [ci_len] is an int*)
let conf_index_finder s_len ci_len =
  if s_len > ci_len then s_len else ci_len

(** [print_beg ()] is the beginning lines for the z-test and regression tables*)
let print_beg () = 
  print_endline "\n\n";
  print_char "-" 90 ""

(** [print_end ()] is the ending lines for the z-tes and regression tables*)
let print_end () = 
  print_char "-" 90 "";
  print_endline "\n\n"

let print_chart var_name data value = 
  print_beg ();
  let ci = pp_conf(confidence_int data value) in let ci_index = String.length ci 
  in let conf_index = conf_index_finder (String.length "[95% Conf. Interval]") 
         ci_index in
  print_layout ([("Variable",90); ("|", 80); ("Obs", 75); ("Mean", 65);
                 ("Std. Err.", 55); ("Std. Dev.", 40); 
                 ("[95% Conf. Interval]", conf_index)]) 90 "" " ";
  print_layout ([("+",80)]) 90 "" "-";
  print_layout ([(var_name, 90); ("|",80);
                 (string_of_int(List.length data), 75); 
                 (string_of_float (round_float(mean data)), 65);
                 (string_of_float (round_float(test_stat data value)), 55); 
                 (string_of_float(round_float(std_dev data)),40); 
                 (ci,conf_index)]) 90 "" " ";
  print_end()

(** [find_name z] is the variable name of z
    Requires: [z] is an (string*float list)*)
let find_name z =
  match z with 
  |(n,d)-> n

(** [find_data z] is the data of variable z
    Requires: [z] is an (string*float list)*)
let find_data z =
  match z with 
  | (n,d) -> d

(** [reg_print y_name] prints the line for the beginning of the regression 
    tables
    Requires: [y_name] is an string*)
let reg_print y_name= 
  print_layout ([(y_name, 90); ("|",80); ("Coef.", 75); ("Std. Err.", 65);
                 ("z", 50); ("P>|t|", 40); ("[95% Conf. Interval]", 25)]) 90 "" " ";
  print_layout ([("+",80)]) 90 "" "-"

(** [print_line_reg var_name var_c std_err] prints the standard line for each 
    independent variable and constant in the regression chart
    Requires: [var_name] is an string
    Requires: [var_c] is an string
    Requires: [std_err] is an string*)
let print_line_reg var_name var_c std_err= 
  print_layout ([(var_name, 90); ("|",80); (var_c, 75); (std_err, 65); 
                 ("---", 50); ("---", 40); ("---", 25)]) 90 "" " "

let form_reg_ls_3 y x2 x1 = 
  print_beg ();
  let y_name = find_name y in let x2_name = find_name x2 in 
  let x1_name = find_name x1 in
  let yd = find_data y in let x2d = find_data x2 in let x1d = find_data x1 in 
  reg_print(y_name);
  let b2 = string_of_float(round_float((b2_3 x2d x1d yd))) in let 
    b1 = string_of_float(round_float((b1_3 x2d x1d yd))) in
  let a = string_of_float(round_float((a_3 x2d x1d yd))) in
  print_line_reg x2_name b2 "---";
  print_line_reg x1_name b1 "---";
  print_line_reg "_cons" a "---";
  print_end ()

let form_reg_ls_2 y x = 
  print_beg ();
  let y_name = find_name y in let x_name = find_name x in
  let yd = find_data y in let xd = find_data x in
  reg_print (y_name);
  let b = string_of_float(round_float((b_2 xd yd))) in 
  let a = string_of_float(round_float((a_2 xd yd))) in 
  print_line_reg x_name b (string_of_float (b2_std_err yd xd));
  print_line_reg "_cons" a (string_of_float (a2_std_err yd xd));
  print_end()