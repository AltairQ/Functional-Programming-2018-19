let rec fix f x = f (fix f) x ;;

let fact = fix (
	fun f-> fun n-> if n = 0 then 1 else n*(f (n-1))
);;

(* needs -rectypes *)
(* let fix f g = (fun x a -> f (x x) a) (fun x a -> f (x x) a) g *)

let fix_ycomb f = (fun (`X x) -> f(x (`X x))) (`X(fun (`X x) y -> f(x (`X x)) y));;


let fact_ref =
	let f = ref (fun x->x) in
	let trec =		
			fun n -> 
				if n = 0 then 1
			else
				n * ((!f) (n-1))		
	in
		f := trec;
		trec;;

let rec fix_ref =
	let self = ref (fun f -> fun a -> f a) in
	let foo = 
		fun f -> fun x ->
			f (!self f) x in
	self := foo;
	foo;;

