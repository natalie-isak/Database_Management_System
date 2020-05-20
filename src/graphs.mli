(* [run] starts the user input program for graphing two data set columns. 
   Raises: [InvalidColor] if the user enters a color not in the gnuplot library 
   Raises: [IncompleteData] if the user tries to graph two columns that are 
   not the same size *)
val run: Functions.t -> unit -> unit
