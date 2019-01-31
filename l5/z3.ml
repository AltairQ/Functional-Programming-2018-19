type void = {nic : 'a . 'a} ;;
type truth = Unit;;
type 'a neg = 'a -> void ;;
type ('a, 'b) conj = 'a * 'b;;
type ('a, 'b) dysj = Left of 'a | Right of 'b;; 
type ('a, 'b) impl = 'a -> 'b;;

type ('a, 'b) provable =
	| And of ('a, 'b) conj
	| Or of ('a, 'b) dysj
	| Imply of ('a, 'b) impl ;;


(* type () ex_proof =  *)