let rec suffixes = (function
| [] -> [[]]
| (x::xs) as l -> l::(suffixes xs));;

let prefixes lst =
	let rec hlp acc rest = match rest with
	| [] -> [acc]
	| (x::xs) -> [acc]@(hlp (acc@[x]) xs)
in
	hlp [] lst;;


