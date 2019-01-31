let partition pred lst = 
	let rec hlp a yes no =
		match a with
		| [] -> (yes,no)
		| (x::xs) -> if (pred x) then (hlp xs (x::yes) no) else (hlp xs yes (x::no))
	in
	hlp lst [] [];; 

let rec qsort comp a =
	match a with
	 | [] -> []
	 | (x::xs) -> 
	 	let hi, low = partition (fun y-> comp x y ) xs in
	 		(qsort comp low)@[x]@(qsort comp hi);;