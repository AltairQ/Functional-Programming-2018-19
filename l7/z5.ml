type jpack = {id : int; mutable visited: bool} ;;
type 'a lnode = {item: 'a; mutable next: 'a lnode} ;;

let mk_circular_list e =
	let rec x = {item=e; next=x}
		in x;;

let insert_head e l =
	let x = {item=e; next=l.next} in
		l.next <- x; l;;

let insert_tail e l =
	let x = {item=e; next=l.next} in
		l.next <- x; x;;

let jhlp i =
	{id=i; visited=false}

let mk_circular_n n =
	let rec aux i =
		if i = n then mk_circular_list (jhlp n) else
			insert_head (jhlp i) (i+1 |> aux)
	in
aux 1;;

let joseph n m =
	let rec solve ci node gcnt=
		if gcnt = n then []
		else
			if node.item.visited then solve ci (node.next) gcnt
		else
			if ci = m then begin
				node.item.visited <- true;
				(node.item.id) :: (solve 1 (node.next) (gcnt+1))
			end
		else
			solve (ci+1) (node.next) gcnt
in
let cl = mk_circular_n n in 
	solve 0 cl 0;;