(* In this test file, we added tests to ensure that all of our functions were 
   working properly. Our methedology was to write a function and then add test 
   cases here. We would consequently continue fixing our function and then adding
   more test cases until we were confident in our results. For coming up with 
   test cases, we choses cases that ensured that all possibilities were covered 
   (meaning, every branch of an if statement, every match possibility, etc.). In 
   addition, we also wrote test cases for all the edge cases (for example, an empty
   list, a list with one element, a list with many elements, etc). Our test cases
   were white box tests because we knew the innner workings of our project as
   we wrote our test suite in test.ml. All parts of the system were 
   tested with OUnit functions (including all math functions, regular functions, 
   and RSA functions) EXCEPT for graphing functionality, which had to be manually 
   tested with REPL AND the command functions, which were manually tested with 
   REPL as well AND the hypothesis tests and regression analysis because they 
   were not completely implemented as part of the excellent scope. We believed 
   that this was the best approach because we felt confident that the style of 
   testing from A2 was effective and efficient. This is largely because all the 
   test cases were automated, which made it easy to write cases quickly. It 
   also made it easy to see which cases we were missing this way and continue 
   looking for edge cases to add. Finally, we combined this test suite with 
   manual testing in REPL to ensure that our command file and graph file work. 
   This was the easiest way to do so since it is extremely difficult to add 
   automated test cases that test REPL user interaction. Finally, another 
   benefit to our testing style is that we also tested that our funcitons 
   raised the correct errors when a bad input was given. This was extremely 
   important thing to do because it ensured that our program did not break if
    a bad input was given. *)

open Functions
open Command
open OUnit2
open Rsa
open Num
open Mathfunc

let dat1 = read_csv "dat1.csv"
let dat2 = read_csv "dat2.csv"
let dat3 = read_csv "dat3.csv"
let dat4 = read_csv "dat4.csv"
let dat5 = read_csv "dat5.csv"
let dat6 = read_csv "dat6.csv"
let dat7 = read_csv "dat7.csv"
let dat8 = read_csv "dat8.csv"
let dat9 = read_csv "dat9.csv"
let dat10 = read_csv "dat10.csv"
let dat11 = read_csv "dat11.csv"
let dat12 = read_csv "dat12.csv"
let dat13 = read_csv "dat13.csv"

let make_num_of_rows_tests 
    (name : string)
    (data : t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> assert_equal expected_output (num_of_rows data))

let make_num_of_cols_tests 
    (name : string)
    (data : t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> assert_equal expected_output (num_of_cols data))

let make_shape_tests 
    (name : string)
    (data : t) 
    (expected_output : int list) : test = 
  name >:: (fun _ -> assert_equal expected_output (shape data))

let make_read_row_tests 
    (name : string)
    (data : t) 
    (index : int)
    (expected_output : float list) : test = 
  name >:: (fun _ -> assert_equal expected_output (read_row data index))

let make_read_row_tests_err 
    (name : string)
    (data : t) 
    (index : int) : test = 
  name >:: (fun _ -> assert_raises Functions.(InvalidIndex) 
               (fun() -> read_row data index))

let make_insert_row_tests 
    (name : string)
    (data : t) 
    (add : float list)
    (index : int)
    (expected_output : t) : test = 
  name >:: (fun _ -> assert_equal expected_output (insert_row data add index))

let make_insert_row_tests_err 
    (name : string)
    (data : t) 
    (add : float list)
    (index : int) : test = 
  name >:: (fun _ -> assert_raises Functions.InvalidIndex  
               (fun() -> insert_row data add index))

let make_delete_row_tests 
    (name : string)
    (data : t) 
    (index : int)
    (expected_output : t) : test = 
  name >:: (fun _ -> assert_equal expected_output (delete_row data index))

let make_delete_row_tests_err
    (name : string)
    (data : t) 
    (index : int) : test = 
  name >:: (fun _ -> assert_raises Functions.InvalidIndex  
               (fun() -> delete_row data index))

let make_read_col_tests 
    (name : string)
    (data : t) 
    (index : int)
    (expected_output : float list) : test = 
  name >:: (fun _ -> assert_equal expected_output (read_col data index))

let make_read_col_tests_err
    (name : string)
    (data : t) 
    (index : int) : test = 
  name >:: (fun _ -> assert_raises (Functions.InvalidIndex)  
               (fun() -> read_col data index))

let make_insert_col_tests 
    (name : string)
    (data : t) 
    (add : float list)
    (index : int)
    (expected_output : t) : test = 
  name >:: (fun _ -> assert_equal expected_output (insert_col data add index))

let make_insert_col_tests_err
    (name : string)
    (data : t) 
    (add : float list)
    (index : int) : test = 
  name >:: (fun _ -> assert_raises (Functions.InvalidIndex)  
               (fun() -> insert_col data add index))

let make_delete_col_tests 
    (name : string)
    (data : t) 
    (index : int)
    (expected_output : t) : test = 
  name >:: (fun _ -> assert_equal expected_output (delete_col data index))

let make_delete_col_tests_err 
    (name : string)
    (data : t) 
    (index : int) : test = 
  name >:: (fun _ -> assert_raises (Functions.InvalidIndex)  
               (fun() -> delete_col data index))
let make_mean_tests 
    (name : string)
    (data : t) 
    (expected_output : float) : test = 
  name >:: (fun _ -> assert_equal expected_output (mean (gen_data_ls data)))

let make_mean_tests_err 
    (name : string)
    (data : float list)  : test = 
  name >:: (fun _ -> assert_raises (Mathfunc.EmptyList) (fun() ->(mean data)))

let make_median_tests 
    (name : string)
    (data : t) 
    (expected_output : float) : test = 
  name >:: (fun _ -> assert_equal expected_output (median (gen_data_ls data)))

let make_median_tests_err 
    (name : string)
    (data : float list)  : test = 
  name >:: (fun _ -> assert_raises (Mathfunc.EmptyList) (fun() ->(median data)))

let make_mode_tests 
    (name : string)
    (data : t) 
    (expected_output : string) : test = 
  name >:: (fun _ -> assert_equal expected_output 
               (pp_ls (mode (gen_data_ls data))))

let make_mode_tests2
    (name : string)
    (data : float list) 
    (expected_output : string) : test = 
  name >:: (fun _ -> assert_equal expected_output (pp_ls (mode data)))

let make_variance_tests 
    (name : string)
    (data : t) 
    (expected_output : float) : test = 
  name >:: (fun _ -> assert_equal expected_output 
               (round_float (variance (gen_data_ls data))))    

let make_variance_tests_err 
    (name : string)
    (data : float list)  : test = 
  name >:: (fun _ -> assert_raises (Mathfunc.EmptyList) (fun() ->(variance data)))

let make_std_dev_tests 
    (name : string)
    (data : t) 
    (expected_output : float) : test = 
  name >:: (fun _ -> assert_equal expected_output 
               (round_float(std_dev (gen_data_ls data))))        

let make_std_dev_tests_err 
    (name : string)
    (data : float list)  : test = 
  name >:: (fun _ -> assert_raises (Mathfunc.EmptyList) (fun() ->(std_dev data)))


let make_add_data_tests 
    (name : string)
    (data : float list) 
    (num: float)
    (expected_output : float list) : test = 
  name >:: (fun _ -> assert_equal expected_output (add_data data num))

let make_mult_data_tests 
    (name : string)
    (data : float list) 
    (num: float)
    (expected_output : float list) : test = 
  name >:: (fun _ -> assert_equal expected_output (mult_data data num))
let pub_k_1 = pub_key 7 11 13
let priv_k_1 = priv_key 7 11 13
let pub_k_2 = pub_key 7 3 7
let priv_k_2 = priv_key 7 3 7
let pub_k_3 = pub_key 7 23 37
let priv_k_3 = priv_key 7 23 37

let make_pub_k
    (name : string)
    (e : int) 
    (p : int)
    (q : int)
    (expected_output : int * int) : test = 
  name >:: (fun _ -> assert_equal expected_output (pub_key e p q))

let make_priv_k
    (name : string)
    (e : int) 
    (p : int)
    (q : int)
    (expected_output : int * int) : test = 
  name >:: (fun _ -> assert_equal expected_output (priv_key e p q))

let make_encrypt_dat
    (name : string)
    (data : t) 
    (pub_k : int*int)
    (expected_output : t) : test = 
  name >:: (fun _ -> assert_equal expected_output (encrypt_dat data pub_k))

let make_decrypt_dat
    (name : string)
    (data : t) 
    (priv_k : int*int)
    (expected_output : t) : test = 
  name >:: (fun _ -> assert_equal expected_output (decrypt_dat data priv_k))

let make_encrypt_num 
    (name : string)
    (num : float)
    (pub_k : int*int)
    (expected_output : float) : test = 
  name >:: (fun _ -> assert_equal expected_output (encrypt_num num pub_k)) 

let make_decrypt_num 
    (name : string)
    (num : float)
    (priv_k : int*int)
    (expected_output : float) : test = 
  name >:: (fun _ -> assert_equal expected_output (decrypt_num num priv_k)) 

let main_tests = [
  make_num_of_rows_tests "counting num of rows dat1" dat1 5;
  make_num_of_cols_tests "counting num of cols dat1" dat1 5;
  make_shape_tests "finding the shape of dat1" dat1 [5;5]; 
  make_read_row_tests "returning the 1st row of dat1" dat1 0 [0.;1.;2.;3.;4.]; 
  make_read_row_tests "returning a middle row of dat1" dat1 1 [5.;6.;7.;8.;9.];
  make_read_row_tests "returning the last row of dat1" dat1 4 [20.;21.;22.;23.;24.];
  make_read_row_tests_err "out of bounds index dat1" dat1 20;
  make_read_row_tests_err "out of bounds index dat4" dat4 4;
  make_insert_row_tests "inserting to beginning of dat1" dat1 [7.;7.;7.;7.;7.] 0 dat3; 
  make_insert_row_tests "inserting to beginning of dat4" dat4 [7.;7.;7.;7.;7.] 0 dat2;
  make_insert_row_tests "inserting to middle of dat1" dat1 [7.;7.;7.;7.;7.] 3 dat5;
  make_insert_row_tests "inserting to end of dat3" dat3 [7.;7.;7.;7.;7.] 6 dat10;
  make_insert_row_tests_err "out of bounds index dat1" dat1 [1.;2.;3.;4.;5.] 20; 
  make_insert_row_tests_err "out of bounds index dat4" dat4 [1.;2.;3.;4.;5.] 1;
  make_insert_row_tests_err "out of bounds index dat3 - at edge" dat3 [1.;2.;3.;4.;5.] 7; 
  make_delete_row_tests "deleting all of dat2" dat2 0 dat4;
  make_delete_row_tests "deleting first row of dat3" dat3 0 dat1; 
  make_delete_row_tests "deleting middle row of dat3" dat5 3 dat1; 
  make_delete_row_tests "deleting last row of dat10, edge" dat10 6 dat3;
  make_delete_row_tests_err "deleting from an dat4" dat4 0;
  make_delete_row_tests_err "deleting from an dat4, high index" dat4 10;
  make_delete_row_tests_err "deleting from an dat3, edge index" dat3 6;
  make_read_col_tests "reading the 1st row of dat1" dat1 0 [0.;5.;10.;15.;20.];
  make_read_col_tests "reading a middle col of dat1" dat1 3 [3.;8.;13.;18.;23.];
  make_read_col_tests "reading the last col of dat1" dat1 4 [4.;9.;14.;19.;24.];
  make_read_col_tests "reading a middle col of dat2" dat2 4 [7.];
  make_read_col_tests_err "reading an empty csv, dat4" dat4 10;
  make_read_col_tests_err "invalid index, too high for dat1" dat1 10; 
  make_insert_col_tests "inserting a col to the beginning of dat1" dat1 [0.;0.;0.;0.;0.] 0 dat6;  
  make_insert_col_tests "inserting a col to the middle of dat1" dat1 [0.;0.;0.;0.;0.] 4 dat7;  
  make_insert_col_tests "inserting a col to the end of dat1" dat1 [0.;0.;0.;0.;0.] 5 dat11;  
  make_insert_col_tests "inserting a col to dat4" dat4 [0.;0.;0.;0.;0.] 0 dat12;
  make_insert_col_tests_err "inserting a col to dat12 too high" dat12 [1.;2.] 10;
  make_insert_col_tests_err "inserting a col to dat1 at edge" dat1 [1.;2.;3.;4.;5.] 6;
  make_insert_col_tests_err "inserting a col to empty dat4 but not at 0 index" dat4 [1.;2.;3.;4.;5.] 6;
  make_delete_col_tests "deleting a col at the beginning of dat1" dat1 0 dat8; 
  make_delete_col_tests "deleting a col at the middle of dat1" dat1 1 dat9;
  make_delete_col_tests "deleting a col at the end of dat11" dat11 5 dat1;
  make_delete_col_tests_err "deleting from an empty" dat4 0;
  make_delete_col_tests_err "deleting from an incorrect array, dat1" dat1 100;
  make_delete_col_tests_err "deleting from an edge index, dat1" dat1 5; 
  make_pub_k "testing 11 13 pub key" 7 11 13 (7,143); 
  make_priv_k "testing 11 13 priv key" 7 11 13 (103,143); 
  make_pub_k "testing 2737 419 541 pub key" 2737 419 541 (2737,226679); 
  make_priv_k "testing 2737 419 541 priv key" 2737 419 541 (46513,226679);
  make_encrypt_num "test 432.0" 432.0 pub_k_1 42.0; 
  make_encrypt_num "test 11.0" 24.0 pub_k_1 106.0; 
  make_encrypt_num "test 54.0" 54.0 pub_k_2 12.0; 
  make_encrypt_num "test 7.0" 7.0 pub_k_3 626.0; 
  make_encrypt_num "test 7382.0" 7382.0 pub_k_3 505.0; 
  make_decrypt_num "test 213.0" 213.0 priv_k_3 432.0; 
  make_decrypt_num "test 329.0" 329.0 priv_k_3 34.0; 
  make_encrypt_dat "test dat2" dat2 pub_k_3 dat13; 
  make_decrypt_dat "test dat13" dat13 priv_k_3 dat2; 
]

let math_func_tests = [
  (** Tests for mathfunc*)
  make_mean_tests "mean dat2" dat2 7.;
  make_mean_tests "mean dat1" dat1 12.;
  make_mean_tests "mean dat12" dat12 0.;
  make_mean_tests_err "mean empty_ls" [];

  make_median_tests "median dat1 odd count" dat1 12.;
  make_median_tests "median dat3 even count" dat3 9.5;
  make_median_tests_err "median empty_ls" [];

  make_mode_tests "mode with repeats and other elements" dat6 "[0.; ]";
  make_mode_tests2 "mode one element no repeats" ([1.]) "[1.; ]";
  make_mode_tests2 "mode three elements no repeats" ([1.;2.;3.]) "[3.; 1.; 2.; ]";
  make_mode_tests2 "mode repeat elements"  ([0.;0.;0.;0.;0.]) "[0.; ]";
  make_mode_tests2 "mode empty" ([]) "[]";

  make_variance_tests "variance" (dat7) (63.33333);                               
  make_variance_tests_err "variance no elements err" [];

  make_std_dev_tests "variance" (dat7) (7.95822);                                
  make_std_dev_tests_err "stdev no elements err" [];

  make_add_data_tests "add_data" [] 0. [];
  make_add_data_tests "add_data" [] 1. [];
  make_add_data_tests "add_data" [1.;2.] 0. [1.;2.];
  make_add_data_tests "add_data" [1.;2.] 0.5 [1.5;2.5];
  make_add_data_tests "add_data" [1.;2.] (-1.) [0.;1.];

  make_mult_data_tests "mult_data" [1.;2.] (1.) [1.;2.];
  make_mult_data_tests "mult_data" [1.;2.] (-1.) [-1.;-2.];
  make_mult_data_tests "mult_data" [1.;2.] (2.) [2.;4.];
  make_mult_data_tests "mult_data" [1.;2.] (0.) [0.;0.];
  make_mult_data_tests "mult_data" [] (0.) [];
]

let suite =
  "test suite for DBMS"  >::: List.flatten [
    main_tests;
    math_func_tests;
  ]

let _ = run_test_tt_main suite