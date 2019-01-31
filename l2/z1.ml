let rec map f a = match a with
| [] -> []
| (x::xs) -> (f x)::(map f xs);;


let rec sublists a = match a with
| [] -> [[]]
| (x::xs) -> let l = sublists xs in 
				(map (fun y->x::y) l) @ l;;