(* Zaległe zadanie 2 z listy nr 8 *)

module type VERTEX = 
sig
    type vt
    type label 

    val equal : vt -> vt -> bool
    val create : label -> vt
    val label : vt -> label
end

module type EDGE =
sig
    type vt
    type et = vt * vt
    type label

    val equal : et -> et -> bool 
    val create : label -> vt -> vt -> et
    val fst : et -> vt
    val snd : et -> vt
    val label : et -> label
end

module Vertex : (VERTEX with type vt = int and type label = int) = struct
    type vt = int
    type label = int

    let equal a b = a == b
    let create a = a
    let label a = a
end

module Edge : (EDGE with type vt = int and type label = int*int) = struct
    type vt = int
    type et = vt * vt
    type label = et

    let equal a b = a == b
    let create lab a b = (a, b)
    let fst edg = let (a, b) = edg in a
    let snd edg = let (a, b) = edg in b
    let label edg = edg
end

module type GRAPH = 
sig
    type t

    module V : VERTEX
    type vertex = V.vt

    module E : EDGE with type vt = vertex
    type edge = E.et

    val mem_v : t -> vertex -> bool
    val mem_e : t -> edge -> bool
    val mem_e_v : t -> vertex -> vertex -> bool
    val find_e : t -> vertex -> vertex -> edge
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list 
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list

    val empty : t
    val add_e : t -> edge -> t
    val add_v : t -> vertex -> t
    val rem_e : t -> edge -> t
    val rem_v : t -> vertex -> t

    val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_e : (edge   -> 'a -> 'a) -> t -> 'a -> 'a
end



module Graph : (GRAPH with type V.vt = int and type V.label = int and type E.et = (int*int) and type E.label = (int*int)) = 
struct
    let vfc = (module Vertex : VERTEX with type vt = int and type label = int)

    let efc = (module Edge : EDGE with type vt = int and type label = int*int)

    module V = (val vfc : VERTEX with type vt = int and type label = int)
    module E = (val efc : EDGE with type vt = int and type label = int*int)

    type t = {vs : V.vt list; es : E.et list}
    (* type t = ig *)

    type vertex = V.vt
    type edge = E.et

    let rec mem_vlist vlst q = match vlst with
        | [] -> false
        | v::vs -> if (V.equal v q) then true else mem_vlist vs q

    let rec mem_elist elst q = match elst with
        | [] -> false
        | e::es -> if (E.equal e q) then true else mem_elist es q

    let rec mem_vv elst a b = match elst with
        | [] -> failwith "Not found"
        | e::es -> if (E.fst e = a) && (E.snd e = b) then e else mem_vv es a b

    let rec filter_once f lst = match lst with
        | [] -> []
        | x::xs -> if (f x) then xs else x::(filter_once f xs)

    let mem_v graph v =
        mem_vlist graph.vs v
        
    let mem_e graph e =
        mem_elist graph.es e

    let mem_e_v graph a b =
        mem_elist graph.es (E.create (a,b) a b)

    let find_e graph v1 v2 =
        mem_vv graph.es v1 v2

    let succ_e graph v = 
        List.filter (fun e -> E.fst e = v) graph.es

    let pred_e graph v =
        List.filter (fun e -> E.snd e = v) graph.es

    let succ graph v =
        List.map (E.snd) (succ_e graph v)

    let pred graph v = 
        List.map (E.fst) (pred_e graph v)


    let empty = {vs =[]; es = []}

    let add_e graph e =
        {vs = graph.vs; es = e::(graph.es)}

    let add_v graph v =
        {vs = v::(graph.vs); es = graph.es}

    let rem_e graph e = 
        let fltrd = List.filter (fun x -> not (E.equal e x)) graph.es in
            {vs = graph.vs; es = fltrd}

    let rem_v graph v =
        let fltrd = List.filter (fun x -> not (V.equal v x)) graph.vs in
            let efltrd = List.filter (
                        fun x ->
                             (not (V.equal v (E.fst x)))
                             &&
                             (not (V.equal v (E.snd x)))
                        )
                        graph.es in
                {vs = fltrd; es = efltrd}

    let fold_v f graph s = List.fold_right f (graph.vs) s

    let fold_e f graph s = List.fold_right f (graph.es) s

end;;

(* Quick testing *)

let graph_int = ref Graph.empty in
begin
    graph_int := Graph.add_v !graph_int (Graph.V.create 0);
    graph_int := Graph.add_v !graph_int (Graph.V.create 1);
    graph_int := Graph.add_v !graph_int (Graph.V.create 2);
    graph_int := Graph.add_e !graph_int (Graph.E.create (0,1) (Graph.V.create 0) (Graph.V.create 1));
    graph_int := Graph.add_e !graph_int (Graph.E.create (0,2) (Graph.V.create 0) (Graph.V.create 2));

    print_endline "Vertex list:";
    ignore (Graph.fold_v (fun v a -> print_int v; print_newline ()) !graph_int ());

    print_endline "Succ of 0:";
    ignore (List.fold_right (fun v a -> print_int v; print_newline ())
             (Graph.succ !graph_int (Graph.V.create 0))
              ());

    print_endline "Pred of 2";
    ignore (List.fold_right (fun v a -> print_int v; print_newline ())
             (Graph.pred !graph_int (Graph.V.create 2))
              ());

    print_endline "Removing 2...";
    graph_int := Graph.rem_v !graph_int (Graph.V.create 2);


    print_endline "Succ of 0:";
    ignore (List.fold_right (fun v a -> print_int v; print_newline ())
             (Graph.succ !graph_int (Graph.V.create 0))
              ());
end;;


module VEToGraph (V : VERTEX) (E : EDGE with type vt = V.vt ) : (GRAPH with type V.label = V.label and type E.label = E.label) =
struct
    module V = V
    module E = E

    type t = {vs : V.vt list; es : E.et list}

    type vertex = V.vt
    type edge = E.et

    let rec mem_vlist vlst q = match vlst with
        | [] -> false
        | v::vs -> if (V.equal v q) then true else mem_vlist vs q

    let rec mem_elist elst q = match elst with
        | [] -> false
        | e::es -> if (E.equal e q) then true else mem_elist es q

    let rec mem_vv elst a b = match elst with
        | [] -> failwith "Not found"
        | e::es -> if (E.fst e = a) && (E.snd e = b) then e else mem_vv es a b

    let rec filter_once f lst = match lst with
        | [] -> []
        | x::xs -> if (f x) then xs else x::(filter_once f xs)

    let mem_v graph v =
        mem_vlist graph.vs v
        
    let mem_e graph e =
        mem_elist graph.es e

    let rec mem_e_v_aux es a b = match es with
        | [] -> false
        | x::xs -> if ((a = E.fst x) && (b = E.snd x))
                    then true
                    else mem_e_v_aux xs a b

    let mem_e_v graph a b =
        mem_e_v_aux graph.es a b

    let find_e graph v1 v2 =
        mem_vv graph.es v1 v2

    let succ_e graph v = 
        List.filter (fun e -> E.fst e = v) graph.es

    let pred_e graph v =
        List.filter (fun e -> E.snd e = v) graph.es

    let succ graph v =
        List.map (E.snd) (succ_e graph v)

    let pred graph v = 
        List.map (E.fst) (pred_e graph v)


    let empty = {vs =[]; es = []}

    let add_e graph e =
        {vs = graph.vs; es = e::(graph.es)}

    let add_v graph v =
        {vs = v::(graph.vs); es = graph.es}

    let rem_e graph e = 
        let fltrd = List.filter (fun x -> not (E.equal e x)) graph.es in
            {vs = graph.vs; es = fltrd}

    let rem_v graph v =
        let fltrd = List.filter (fun x -> not (V.equal v x)) graph.vs in
            let efltrd = List.filter (
                        fun x ->
                             (not (V.equal v (E.fst x)))
                             &&
                             (not (V.equal v (E.snd x)))
                        )
                        graph.es in
                {vs = fltrd; es = efltrd}

    let fold_v f graph s = List.fold_right f (graph.vs) s

    let fold_e f graph s = List.fold_right f (graph.es) s
end;;

module Tmp = VEToGraph (Vertex) (Edge);;

let rec dfs (type s) (type vt) (module G : GRAPH with type t = s and type V.vt = vt) (graph : s) (v : vt) =
    let rec aux cv =    
        let sub = List.map (fun x -> aux x) (G.succ graph cv) in
            cv :: (List.concat sub)
    in
    aux v;;

let rec bfs (type s) (type vt) (module G : GRAPH with type t = s and type V.vt = vt) (graph : s) (v : vt) =
    let rec aux cv =
        let suc = (G.succ graph cv) in
            let sub = List.map (fun x -> aux x) suc in
                suc @ (List.concat sub)
    in
    v :: (aux v);;

print_string "\n\n ------- Now using VEToGraph (Vertex) (Edge) ---------\n\n";

let graph_int = ref Tmp.empty in
begin
    graph_int := Tmp.add_v !graph_int (Tmp.V.create 0);
    graph_int := Tmp.add_v !graph_int (Tmp.V.create 1);
    graph_int := Tmp.add_v !graph_int (Tmp.V.create 2);
    graph_int := Tmp.add_v !graph_int (Tmp.V.create 3);
    graph_int := Tmp.add_v !graph_int (Tmp.V.create 4);
    graph_int := Tmp.add_v !graph_int (Tmp.V.create 5);
    graph_int := Tmp.add_e !graph_int (Tmp.E.create (0,1) (Tmp.V.create 0) (Tmp.V.create 1));
    graph_int := Tmp.add_e !graph_int (Tmp.E.create (0,2) (Tmp.V.create 0) (Tmp.V.create 2));
    graph_int := Tmp.add_e !graph_int (Tmp.E.create (2,3) (Tmp.V.create 2) (Tmp.V.create 3));
    graph_int := Tmp.add_e !graph_int (Tmp.E.create (2,4) (Tmp.V.create 2) (Tmp.V.create 4));
    graph_int := Tmp.add_e !graph_int (Tmp.E.create (1,5) (Tmp.V.create 1) (Tmp.V.create 5));

    (* 
            0 -> 1 -> 5
             \
              -> 2 -> 3
                  \
                   -> 4

     *)

    print_endline "Vertex list:";
    ignore (Tmp.fold_v (fun v a -> print_int (Tmp.V.label v); print_newline ()) !graph_int ());

    print_endline "Succ of 0:";
    ignore (List.fold_right (fun v a -> print_int (Tmp.V.label v); print_newline ())
             (Tmp.succ !graph_int (Tmp.V.create 0))
              ());

    print_endline "Pred of 2";
    ignore (List.fold_right (fun v a -> print_int (Tmp.V.label v); print_newline ())
             (Tmp.pred !graph_int (Tmp.V.create 2))
              ());

    print_endline "DFS(0)";
    ignore (List.fold_left (fun a v -> print_int (Tmp.V.label v); print_string " ")
            ()
             (dfs (module Tmp) !graph_int (Tmp.V.create 0) )
              );

    print_newline ();

    print_endline "BFS(0)";
    ignore (List.fold_left (fun a v -> print_int (Tmp.V.label v); print_string " ")
            ()
             (bfs (module Tmp) !graph_int (Tmp.V.create 0) )
              );

    print_newline ();

    print_endline "Removing 2...";
    graph_int := Tmp.rem_v !graph_int (Tmp.V.create 2);


    print_endline "Succ of 0:";
    ignore (List.fold_right (fun v a -> print_int (Tmp.V.label v); print_newline ())
             (Tmp.succ !graph_int (Tmp.V.create 0))
              ());
end;;

