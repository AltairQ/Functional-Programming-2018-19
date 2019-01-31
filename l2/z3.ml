let rec merge comp a b = 
	match a,b with
	| _, [] -> a
	| [], _ -> b
	| (x::xs), (y::ys) ->
		if comp x y then
			x :: (merge comp xs b)
		else
			y :: (merge comp a ys);;


