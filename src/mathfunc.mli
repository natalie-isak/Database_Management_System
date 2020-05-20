exception EmptyList

(** [pp_ls ls] returns the float list as a string
    Requires: [ls] is a float list*)
val pp_ls: float list -> string

(** [mean data] is the mean of the elements in data  
    Requires: [data] is a float list                      
    Raises [EmptyList] if data is empty*)
val mean: float list -> float

(** [median d] returns the median of d
    Raises [EmptyList] if [d] is empty
    Requires: [d] is a float list*)
val median: float list -> float 

(**[mode data] returns the mode of [data]
   Requires: [data] is a float list*)
val mode: float list -> float list

(**[variance data] returns variance of [data]
   Requires: [data] is a float list
   Raises [EmptyList] if [data] is empty*)
val variance: float list -> float

(**[std_dev data] returns the standard deviation of [data]
   Requires: [data] is a float list
   Raises [EmptyList] if [data] is empty *)
val std_dev: float list -> float

(** [add_data data num] adds float [num] to each element in [data]
    Requires: [data] is a float list
    Requires: [num] is a float*)
val add_data: float list -> float -> float list 

(**[mult_data data num] multiplies float [num] by each element in [data]
   Requires: [data] is a float list
   Requires: [num] is a float*)
val mult_data: float list -> float -> float list 

(** [print_char c num acc] prints a character [c] [num] times
    Requires: [c] is a string
    Requires: [num] is an int
    Requires: [acc] is a string*)
val print_chart: string -> float list -> float -> unit

(** [form_reg_ls_3 y x2 x1] prints the regression chart for the two variable 
    regression *Note: incomplete implementation
    Requires: [y] is an (string*float) list
    Requires: [x2] is an (string*float) list
    Requires: [x1] is an (string*float) list*)
val form_reg_ls_3: (string*float list) -> (string*float list) -> 
  (string*float list) -> unit

(** [form_reg_ls_2 y x] prints the regression chart for the three variable 
    regression *Note: incomplete implementation
    Requires: [y] is an (string*float) list
    Requires: [x] is an (string*float) list*)
val form_reg_ls_2: (string*float list)-> (string*float list)-> unit

(** [round_float f] is f rounded to 5 decimal places
    Requires: [f] is a float*)   
val round_float: float -> float