(* PF 5.1 14.11.2018 *)

(* ZSP *)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let lhd = function
	LNil -> failwith "lhd"
	| LCons (x, _) -> x
;;

let ltl = function
	LNil -> failwith "ltl"
	| LCons (_, xf) -> xf()
;;

let rec ltake = function
	(0, _) -> []
	| (_, LNil) -> []
	| (n, LCons(x,xf)) -> x::ltake(n-1, xf())
;;

(* ==================== *)

let rec leibniz_nth (k:int) (acc:float) =
	let nextk = if k > 0 then 0-(k+2) else 2-k in
		let nextacc = acc +. 1.0 /. (float_of_int k) in
			LCons (nextacc,
				function () -> leibniz_nth nextk nextacc);;

let leibniz_sum = leibniz_nth 1 0.0;;

print_string "First 20 terms of 4*leibniz_sum";;
List.map (fun x -> x *. 4.0) (ltake (20, leibniz_sum));;

let three_window (f:'a->'a->'a->'b) (ll:'a llist) =
	let rec aux x y rest = match rest with
	| LNil -> LNil
	| LCons(z, xf) -> LCons(f x y z, function () -> aux y z (xf()) )
in
	let h1 = lhd ll in
	let t1 = ltl ll in
	let h2 = lhd t1 in
	let t2 = ltl t1 in
		aux h1 h2 t2;;

let euler_transform x y z =
	z -. ((y-.z)*.(y-.z) /. (x -. 2.0*.y +.z));;

let leibniz_sum2 = three_window euler_transform (leibniz_sum);;

print_string "First 20 terms of 4*leibniz_sum2";;
List.map (fun x -> x *. 4.0) (ltake (20, leibniz_sum2));;


(* using Lazy module *)
type 'a lazylist = LazyNil | LazyCons of 'a * 'a lazylist Lazy.t;;


let lazyhd = function
	LazyNil -> failwith "lhd"
	| LazyCons (x, _) -> x
;;

let lazytl = function
	LazyNil -> failwith "ltl"
	| LazyCons (_, lazy xf) -> xf
;;

let rec lazytake = function
	(0, _) -> []
	| (_, LazyNil) -> []
	| (n, LazyCons(x, lazy xf)) -> x::lazytake(n-1, xf)
;;


let rec lazy_leibniz_nth (k:int) (acc:float) =
	let nextk = if k > 0 then 0-(k+2) else 2-k in
		let nextacc = acc +. 1.0 /. (float_of_int k) in
			LazyCons (nextacc,
				lazy (lazy_leibniz_nth nextk nextacc));;

let lazy_leibniz_sum = lazy_leibniz_nth 1 0.0;;

print_string "First 20 terms of 4*lazy_leibniz_sum";;
List.map (fun x -> x *. 4.0) (lazytake (20, lazy_leibniz_sum));;

let lazy_three_window (f:'a->'a->'a->'b) (ll:'a lazylist) =
	let rec aux x y rest = match rest with
	| LazyNil -> LazyNil
	| LazyCons(z, lazy xf) -> LazyCons(f x y z, lazy(aux y z xf) )
in
	let h1 = lazyhd ll in
	let t1 = lazytl ll in
	let h2 = lazyhd t1 in
	let t2 = lazytl t1 in
		aux h1 h2 t2;;

let euler_transform x y z =
	z -. ((y-.z)*.(y-.z) /. (x -. 2.0*.y +.z));;

let lazy_leibniz_sum2 = lazy_three_window euler_transform (lazy_leibniz_sum);;

print_string "First 20 terms of 4*lazy_leibniz_sum2";;
List.map (fun x -> x *. 4.0) (lazytake (20, lazy_leibniz_sum2));;
