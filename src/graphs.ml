open Base
open Stdlib
open Functions

exception IncompleteData of string
exception InvalidColor

(* [extract_float_list data x y] returns a float float list containing x, y
   points to graph based on two chosen columns in a data set.
   Requires: [data] is a valid array that satisfies rep_ok
   Requires: [x] and [y] are integers
   Raises: [IncompleteData] if data does not contain equal x and y column sizes*)
let extract_float_list (data:Functions.t) (x:int) (y:int) = 
  let x_points = read_col data x in 
  let y_points = read_col data y in 
  let rec extr_helper xfloats yfloats = 
    match xfloats, yfloats with 
    | a::b, c::d -> begin match a::b, c::d with 
        | [], [] -> []
        | x::[], y::[] -> (x, y) :: []
        | x::xs, y::ys -> (x, y) :: (extr_helper xs ys)
        | _ -> [] end
    | _ -> raise (IncompleteData "Your data is incomplete!")
  in extr_helper x_points y_points

(* [string_to_color str] returns a Gnuplot color based on user input for the
   colors of vertices.
   Requires: [str] is a valid color string
   Requires: [index] is any integer 
   Raises: [InvalidColor] if the input string is not a Gnuplot color *)
let string_to_color str = 
  let str_trim = String.trim str in
  match (String.lowercase_ascii str_trim) with 
  | "blue" -> `Blue
  | "cyan" -> `Cyan
  | "green" -> `Green
  | "magenta" -> `Magenta
  | "red" -> `Red
  | "white" -> `White
  | "yellow" -> `Yellow 
  | _ -> raise InvalidColor

(* This module imports the Gnuplot library, which is an external library we used
   to graph our data *)
module Gp = Gnuplot

let run data () =
  try
    print_string "How would you like to title your chart? ";
    let inp_title = read_line () in
    print_string "What would you like to title your X axis? ";
    let inp_x = read_line () in 
    print_string "What would you like to title your Y axis? ";
    let inp_y = read_line () in 
    print_string "What would you like your X column to be? ";
    let inp_xcol = read_line () in 
    print_string "What would you like your Y column to be? ";
    let inp_ycol = read_line () in 
    print_string "What would you like your X maximum to be? ";
    let inp_xmax = read_line () in 
    print_string "What would you like your Y maximum to be? ";
    let inp_ymax = read_line () in 
    print_string "Pick one:blue, cyan, green, magenta, red, white, and yellow. ";
    let inp_col = read_line () in 
    let col = string_to_color (inp_col) in 
    let axes = Gp.Labels.create ~x:inp_x ~y:inp_y () in 
    let gp = Gp.create () in 
    Gp.set gp ~title:inp_title ~use_grid:true ~labels:axes;
    Gp.plot gp ~range:(Gp.XY (-0., (float_of_string (String.trim inp_xmax)), -0., 
                              (float_of_string (String.trim inp_ymax)))) 
      (Gp.Series.points_xy (extract_float_list data 
                              (int_of_string (String.trim inp_xcol)) 
                              (int_of_string (String.trim inp_ycol)))
         ~color:col);
    Unix.sleep 10;
    Gp.close gp
  with 
  | InvalidColor -> print_string "You entered a bad color!"
  | IncompleteData str -> print_string "Incomplete! Try loading different data."