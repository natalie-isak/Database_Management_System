(* raised when an invalid input is given *)
exception Incorrect_Input of string

(* raised when the modular inverse does not exist *)
exception DNE

(* [pub_key e p q] calculates the public key
   Requires: [e] a number  co-prime to phi
   Requires: [p] a large prime
   Requires: [q] a large prime *)
val pub_key: int -> int -> int -> int * int 

(* [priv_key e p q] calculates the private key
   Requires: [e] a number co-prime to phi
   Requires: [p] a large prime
   Requires: [q] a large prime *)
val priv_key: int -> int -> int -> int * int 

(* [encrypt_dat m pub_k] returns the array with the encryption
   Requires: [dat] the float array you are working with 
   Requires: [pub_k] tuple containing the public key *)
val encrypt_dat: Functions.t -> int*int -> Functions.t

(* [decrypt_dat m priv_k] returns the array with the decryption
   Requires: [dat] the float array you are working with 
   Requires: [priv_k] tuple containing the priv key *)
val decrypt_dat: Functions.t -> int*int -> Functions.t

(* [encrypt_num m pub_k] calculates the private key
   Requires: [m] a positive float you would like to encrypt
   Requires: [pub_k] tuple containing the public key *)
val encrypt_num: float -> int*int -> float

(* [encrypt m pub_k] calculates the private key
   Requires: [m] a positive float you would like to encrypt
   Requires: [pub_k] tuple containing the public key *)
val decrypt_num: float -> int*int -> float