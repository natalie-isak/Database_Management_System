open Random 
open Functions 
open Num

exception Incorrect_Input of string
exception DNE

(* [euclid_gcd a b] returns the GCD using Euclid's algorithm
   Requires: [a] any positive integer
   Requires: [b] any positive integer *)
let rec euclid_gcd a b = 
  if b <> 0 then euclid_gcd b (a mod b)
  else a

(* [rel_prime a b] determines if 2 numbers are relatively prime.
   Requires: [a] any positive integer
   Requires: [b] any positive integer 
   Raises: [Not_Rel_Prime "The prime numbers you gave were not co-prime" if 
   false*)
let rel_prime a b = 
  if euclid_gcd a b <> 1 then false
  else true 

(* [mult_inv a b] is a helper function for finding the mod_inv
   Requires: [a] any positive integer
   Requires: [b] any positive integer *)
let rec mult_inv a b = 
  if a = 0 then (b,0,1)
  else let (g, x, y) = mult_inv (b mod a) a in (g,y-(b/a)*x,x)

(* [mod_inv a b] determines the multiplicative inverse of two numbers using 
   Euclid's extended algorithm
   Requires: [a] any positive integer
   Requires: [b] any positive integer *)
let mod_inv a b =
  let (g, x, y) = mult_inv a b in 
  if g <> 1 then raise DNE
  else x mod b

(* [is_prime n] determines if a number is prime or not
   Requires: [a] any positive integer *)
let is_prime n =
  let rec help d =
    d < 2 || (n mod d <> 0 && help (d-1)) in help (n/2)

let pub_key e p q = 
  if is_prime p = false then raise (Incorrect_Input "p must be a prime number")
  else if is_prime q = false then raise 
      (Incorrect_Input "q must be a prime number")
  else if rel_prime e ((p-1) * (q-1)) = false then 
    raise (Incorrect_Input "e must be relatively prime to phi")
  else (e, p*q)

let priv_key e p q=
  if is_prime p = false then raise (Incorrect_Input "p must be a prime number")
  else if is_prime q = false then 
    raise (Incorrect_Input "q must be a prime number")
  else if rel_prime e ((p-1) * (q-1)) = false then 
    raise (Incorrect_Input "e must be relatively prime to phi")
  else let _, a, _ = mult_inv e ((p-1) * (q-1)) in 
    if a<0 then (a+((p-1)*(q-1)), p*q)
    else (a, p*q)

let encrypt_num m pub_k = 
  let (e, n) = pub_k in 
  let e_b = Num.num_of_int e in
  let n_b = Num.num_of_int n in 
  let m_b = m |> int_of_float |> Num.num_of_int in
  (Num.mod_num (Num.power_num (m_b) e_b) n_b) |> Num.int_of_num |> float_of_int

let encrypt_dat dat pub_k = 
  let rec help dat pub_k counter = 
    if counter < 0 then dat  
    else let orig = read_row dat counter in 
      let replace = List.map (fun x -> encrypt_num x pub_k) orig in 
      let new_dat = delete_row dat counter in 
      help (insert_row (new_dat) replace counter) pub_k (counter - 1)  
  in (help dat pub_k (num_of_rows dat - 1)) 

let decrypt_num c priv_k = 
  let (d, n) = priv_k in 
  let c_b = c |> int_of_float |> Num.num_of_int in
  let d_b = Num.num_of_int d in
  let n_b = Num.num_of_int n in 
  Num.mod_num (Num.power_num c_b d_b) n_b |> Num.int_of_num |> float_of_int 

let decrypt_dat dat priv_k = 
  let rec help dat priv counter = 
    if counter < 0 then dat  
    else let orig = read_row dat counter in 
      let replace = List.map (fun x -> decrypt_num x priv_k) orig in 
      let new_dat = delete_row dat counter in 
      help (insert_row (new_dat) replace counter) priv_k (counter - 1)  
  in (help dat priv_k (num_of_rows dat - 1)) 