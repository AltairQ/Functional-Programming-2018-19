type ('a, 'b) fun_mem = ('a -> 'b)*(('a, 'b) Hashtbl.t ) ;;

type mytest = MTa | MTb;;

let fun_mem_create (f : 'a -> 'b) : ('a, 'b) fun_mem =
	f, (Hashtbl.create 100) ;;

let fun_mem_fetch (fm : ('a, 'b) fun_mem) (arg : 'a) =
	let hsh = snd fm in
		try
			Hashtbl.find hsh arg
		with
		| Not_found -> begin
			let y = (fst fm) arg in
				print_endline "nf!";
				Hashtbl.add hsh arg y; y end ;;

let fun_mem_add (fm : ('a, 'b) fun_mem) (arg : 'a) =
	let hsh = snd fm in
		let f = fst fm in 
			let y = f arg in
				Hashtbl.add hsh arg y; y;;


let rec fib n = 
	if n = 0 then 1 else n * (n-1 |> fib);;

let fib_memo = fun_mem_fetch (fun_mem_create fib);;

(* not allowed because reasons *)
(* let rec fib_memo2 = fun_mem_fetch (fun_mem_create (
	fun n -> if n = 0 then 1 else n* (n-1 |> fib_memo2)
)) ;; *)

let rec fib_memo2 =
	let mem = Hashtbl.create 100 in begin
	fun n->
		try Hashtbl.find mem n
	with
		Not_found -> (
			if n = 0 then 1 else
			let newfib = n * (n-1 |> fib_memo2) in
				Hashtbl.add mem n newfib; print_endline "nf!";
			newfib
		)
	end ;;