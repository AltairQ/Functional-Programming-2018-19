(* Lista 3 efes *)

(* Zadanie 3.1 *)
(* Wersja ogonowa *)
let rec atoi : char list -> int =
	let rec aux acc cl =
		match cl with
		| [] -> acc
		| nc::ncs -> let n = (int_of_char nc) - 48 in
			aux (10*acc + n) ncs
	in
		aux 0 ;;

(* Wersja fold_left *)
let atoi_fl : char list -> int =
	List.fold_left (fun acc n -> acc*10 + (int_of_char n) - 48) 0 ;;

(* TDD *)
assert (atoi ['0'] = 0);;
assert (atoi ['1';'2';'3';'4';] = 1234);;
assert (atoi ['0';'1';'2';'3';'4';] = 1234);;

assert (atoi_fl ['0'] = 0);;
assert (atoi_fl ['1';'2';'3';'4';] = 1234);;
assert (atoi_fl ['0';'1';'2';'3';'4';] = 1234);;

(* Zadanie 3.2 *)
(* Wersja ogonowa *)
let polynomial : float list -> float -> float = 
	let rec aux acc cls x = match cls with
	| [] -> acc
	| c::cs -> aux (acc *. x +. c) cs x
	in aux 0. ;;

(* Wersja fold_left *)
let polynomial_fl cl x : float = 
	List.fold_left (fun acc cf -> acc *. x +. cf) 0. cl ;;

(* TDD *)
assert (polynomial [0.;0.;0.] 42. = 0.);;
assert (polynomial [1.;0.;0.] 4. = 16.);;
assert (polynomial [1.;0.;-1.;2.] 42. = 74048.);;

assert (polynomial_fl [0.;0.;0.] 42. = 0.);;
assert (polynomial_fl [1.;0.;0.] 4. = 16.);;
assert (polynomial_fl [1.;0.;-1.;2.] 42. = 74048.);;

(* Zadanie 3.3 *)
(* Wersja ogonowa *)

let foldr_tail f lst b =
	let rec aux acc l = match l with
	| [] -> acc
	| x::xs -> aux (f x acc) xs
in
	aux b (List.rev lst) ;; 

(* Wersja fold_left *)
let foldr_fl f lst b = 
	List.fold_left (fun x y -> f y x) b (List.rev lst) ;;

(* TDD *)
assert (foldr_tail (List.cons) [1;2;3;4] [] = [1;2;3;4]);;
assert (foldr_fl   (List.cons) [1;2;3;4] [] = [1;2;3;4]);;

(* Zadanie 3.4 *)
let ins_everywhere_fr (x:'a) (lst:'a list) : ('a list list) =
	List.fold_right
		(fun el (res, sufx) ->
			let ex_res = (List.map (fun l -> el::l) res) in
			( (x::el::sufx) :: ex_res ,
			el::sufx)
		)
		lst ([[x]], [])
	|> fst ;;

let ins_everywhere_fl (x:'a) (lst:'a list) : ('a list list) =
	List.fold_left
		( fun (res, prefx) el ->
			let ex_res = (List.map (fun l -> l @ [el]) res) in
			(
				(prefx @ [el; x]) :: ex_res,
				prefx @ [el]				
			)
		)
	([[x]], []) lst
	|> fst;;

(* TDD *)
assert (ins_everywhere_fr 'a' ['1';'2';'3'] =
		[['a'; '1'; '2'; '3'];
		['1'; 'a'; '2'; '3'];
		['1'; '2'; 'a'; '3'];
 		['1'; '2'; '3'; 'a']]);;

assert (ins_everywhere_fl 'a' ['1';'2';'3']=
		[['1'; '2'; '3'; 'a'];
		['1'; '2'; 'a'; '3'];
		['1'; 'a'; '2'; '3'];
 		['a'; '1'; '2'; '3']]);;

(* Zadanie 5 *)
let polynomial2_rec (v:float list) (x:float) : float =
	let rec aux vec cx = match vec with
	| [] -> 0.
	| coeff::rest -> coeff *. cx +. aux rest (cx *. x)
in
	aux v 1.;;

let polynomial2_fr (v:float list) (x:float) : float =
	List.fold_right	(fun c s ->	c +. (s *. x)) v 0. ;;

let polynomial2_tail (v:float list) (x:float) : float =
	let rec aux acc vec cx = match vec with
	| [] -> acc
	| coeff::rest -> aux (acc +. (coeff *. cx)) rest (cx *. x)
in
	aux 0. v 1.;;

let polynomial2_fl (v:float list) (x:float) : float =
	List.fold_left
		(fun (sum, cx) coeff ->
			( sum +. coeff *. cx,
				cx *. x		
			)
		)
		(0., 1.) v
	|> fst;;

(* TDD *)
assert (polynomial2_rec (List.rev [0.;0.;0.] ) 42. = 0.);;
assert (polynomial2_rec (List.rev [1.;0.;0.] ) 4. = 16.);;
assert (polynomial2_rec (List.rev [1.;0.;-1.;2.])  42. = 74048.);;

assert (polynomial2_fr (List.rev [0.;0.;0.] ) 42. = 0.);;
assert (polynomial2_fr (List.rev [1.;0.;0.] ) 4. = 16.);;
assert (polynomial2_fr (List.rev [1.;0.;-1.;2.])  42. = 74048.);;

assert (polynomial2_tail (List.rev [0.;0.;0.] ) 42. = 0.);;
assert (polynomial2_tail (List.rev [1.;0.;0.] ) 4. = 16.);;
assert (polynomial2_tail (List.rev [1.;0.;-1.;2.])  42. = 74048.);;

assert (polynomial2_fl (List.rev [0.;0.;0.] ) 42. = 0.);;
assert (polynomial2_fl (List.rev [1.;0.;0.] ) 4. = 16.);;
assert (polynomial2_fl (List.rev [1.;0.;-1.;2.])  42. = 74048.);;

(* Strona 4 *)

type 'a mtx = 'a list list;;

exception Mtx of string;;

(* Zadanie 3.8 *)
type dim = {columns:int; rows:int};;
let mtx_dim (m : 'a mtx) : dim =
	let rows = List.length m and
		columns = List.length (List.hd m) in
		{
			columns = List.fold_left (fun x y -> 
						if x = (List.length y) then 
							x 
						else
							raise (Mtx "Macierz nie jest prostokątna"))
					columns m ;

			rows = rows
		} ;;

(* TDD *)
assert (mtx_dim [[1;2;3;4];[5;6;7;8];[9;10;11;12]] = {columns = 4; rows = 3;});;

try
	let _ = mtx_dim [[1;2;3;4];[5;6;7;8];[9;10;11]] in
	raise (Failure "Input check fail!")
with
	Mtx z -> () ;; 

(* Zadanie 3.9 *)
let mtx_row (row:int) (m:'a mtx) : ('a list) = 
	try 
		List.nth m (row - 1)
	with
		Failure _ -> raise (Mtx "Index out of range") ;;

let mtx_column (column:int) (m:'a mtx) : ('a list) =
	try
		let idx = column-1 in
			List.fold_right
				(fun row xs -> (List.nth row idx)::xs)
				m []
	with
		Failure _ -> raise (Mtx "Index out of range");;

let mtx_elem (column:int) (row:int) (m:'a mtx) : 'a  =
	try
		List.nth (mtx_row row m) (column-1)
	with
		Failure _ -> raise (Mtx "Index out of range");;

(* TDD *)
assert (mtx_row 2 [[1;2;3;4];[5;6;7;8];[9;10;11;12]] = [5;6;7;8]);;
assert (mtx_column 2 [[1;2;3;4];[5;6;7;8];[9;10;11;12]] = [2;6;10]);;
assert (mtx_elem 1 1 [[1;2;3;4];[5;6;7;8];[9;10;11;12]] = 1);;
assert (mtx_elem 2 3 [[1;2;3;4];[5;6;7;8];[9;10;11;12]] = 10);;

(* Zadanie 3.10 *)
let rec traspose (m:'a mtx) : ('a mtx) =
	try
		match m with
		| [] -> []
		| []::xs -> traspose xs
		| (x::xs) :: rows ->
			(x :: List.map (List.hd) rows) :: traspose (xs :: List.map (List.tl) rows)
	with
		_ -> raise (Mtx "Malformed matrix");;

(* TDD *)
assert (traspose [[1;2;3];[4;5;6]] = [[1;4];[2;5];[3;6]])


(* Zadanie 3.11 *)
let mtx_add_row (r1:float list) (r2:float list):(float list)=
	try
		List.fold_right2 
			(fun a b rest ->
				(a+.b)::rest)
			r1 r2 []
	with
		Invalid_argument _ -> raise (Mtx "Incompatible lengths");;


let mtx_add (m1:float mtx) (m2:float mtx):(float mtx) =
	try
		List.fold_right2
			(fun a b rest ->
				(mtx_add_row a b)::rest)
			m1 m2 []
	with
		Invalid_argument _ -> raise (Mtx "Incompatible dimensions");;

(* TDD *)
assert (mtx_add [[ 1.; 2.; 3.];[ 4.; 5.; 6.];[ 7.; 8.; 9.]]
				[[-1.;-2.;-3.];[-4.;-5.;-6.];[-7.;-8.;-9.]] =
				[[ 0.; 0.; 0.];[ 0.; 0.; 0.];[ 0.; 0.; 0.]]);;


(* Zadanie 3.12 *)
let scalar_prod (v1:float list) (v2:float list) : (float) =
	try
		List.fold_left2
			(fun acc a b ->
				acc +. (a *. b))
			0. v1 v2

	with
	| Invalid_argument _ -> raise (Mtx "Incompatible lengths") ;;

let polynomial_scalar (v:float list) (x:float) : (float) =
	let rec aux (n:int) = match n with
		| 0 -> []
		| 1 -> [1.]
		| k -> let tmp = aux (k-1) in
			(x *. List.hd tmp) :: tmp;
	in
		scalar_prod (aux (List.length v)) v;;

(* TDD *)
assert (scalar_prod [1.;2.;3.;4.;5.] [5.;4.;3.;2.;1.] = 35.);;

assert (polynomial_scalar [0.;0.;0.] 42. = 0.);;
assert (polynomial_scalar [1.;0.;0.] 4. = 16.);;
assert (polynomial_scalar [1.;0.;-1.;2.] 42. = 74048.);;

(* Zadanie 3.13 *)
let mtx_apply (m:float mtx) (v:float list) : (float list) =
	List.map
		(fun row ->	scalar_prod row v)
		m ;;

let mtx_mul (m1:float mtx) (m2:float mtx) : (float mtx) =
	let m2t = traspose m2 in
		List.map
			(fun m1r ->
				List.map 
					(fun m2tr ->
						scalar_prod m1r m2tr
					)
					m2t
			)
			m1;;

(* TDD *)
(* 90deg rotate *)
assert (mtx_apply [[0.;-1.];[1.;0.]] [3.;2.] = [-2.;3.]);;
(* inverse check *)
assert (mtx_mul [[ 1.;2.];[3. ; 4.]]
				[[-2.;1.];[1.5;-0.5]] =
				[[ 1.;0.];[0. ; 1.]]);;

