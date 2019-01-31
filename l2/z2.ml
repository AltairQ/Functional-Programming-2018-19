let rec onerot l acc = 
	match l with
	| [] -> acc
	| [x] -> x::acc
	| (x::xs) -> onerot xs (acc@[x]);;


let rec cycle l n = 
	if n = 0 then l else cycle (onerot l []) (n-1);;