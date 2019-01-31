let spawn_pair = 
	let cnt = ref 0 in
		(fun s->
			cnt := !cnt +1;
			s^(string_of_int !cnt)
			) ,
		(fun n-> cnt := n) ;;

let fresh, reset = spawn_pair