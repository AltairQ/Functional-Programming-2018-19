
let menu (opt) =
  let numItems = Array.length opt-1
  in
    begin
      print_string "\n\n=================================================== \n";
      print_string opt.(0);print_newline();
      for i=1 to numItems do  print_int i; print_string (". "^opt.(i)); print_newline() done;
      print_string "\nSelect an option: ";
      flush stdout;
      let choice = ref (read_int())
      in 
	while !choice < 1 || !choice > numItems do 
	  print_string ("Choose number between 1 and " ^ string_of_int numItems ^ ": ");
	  choice := read_int();
	done; 
	!choice
    end
;;

let s = ref (Stack.create());;
let menuItems = Array.make 6 "";;
let quit = ref false;;
let choice = ref 7;;

menuItems.(0) <- "Stack Operations (implementation on mutable lists)";
menuItems.(1) <- "push";
menuItems.(2) <- "top";
menuItems.(3) <- "pop";
menuItems.(4) <- "isEmpty";
menuItems.(5) <- "quit testing";
while not !quit do
  begin
    choice := menu(menuItems);
    match !choice with
	1 ->
	  begin
	    print_string "Stack item = ";
	    Stack.push (read_int(), !s);
	  end  
      | 2 ->
	  begin
	    begin
	      try print_int (Stack.top !s)  with 
		  Stack.Empty m -> print_string ("Exception: "^m);
	    end;
	    print_newline();
	  end
      | 3 ->
	  begin
	    Stack.pop !s;
	    print_string "popped\n";
	  end
      | 4 ->
	    print_string ("Stack is "^(if Stack.isEmpty !s then "" else "not ")^"empty.\n");
      | 5 ->
	    quit := true
      | _ ->
	    print_string "IMPOSSIBLE!!!\n"
  end
done
