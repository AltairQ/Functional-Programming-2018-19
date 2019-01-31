type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref ;;

let rec l2ml (a : 'a list) = 
	match a with
	| x::xs -> let tmp = l2ml xs in LMcons (x, ref tmp)
	| [] -> LMnil ;;

let rec ml2l (a : 'a list_mutable) =
	match a with
	| LMnil -> []
	| LMcons (x, ref_xs) -> let xs = ml2l !ref_xs in x::xs ;;

let rec concat_copy (alm : 'a list_mutable ref) (blm : 'a list_mutable ref) =
	match !alm with
	| LMcons (a, rest) -> let tmp = concat_copy rest blm in LMcons (a, ref tmp)
	| LMnil -> !blm

let rec concat_share (alm : 'a list_mutable ref) (blm : 'a list_mutable ref) =
	let rec aux ar br =
		match !ar with
		| LMcons (_, rrest) -> if !rrest = LMnil then rrest := !blm else aux rrest blm
		| _ -> failwith "oof"
	in
	if !alm = LMnil then
		!blm
	else
		(aux alm blm;
		!alm );;
		
