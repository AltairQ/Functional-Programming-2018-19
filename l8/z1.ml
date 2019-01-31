module type PQUEUE = 
sig
    type priority
    type 'a t

    exception EmptyPQueue

    val empty : 'a t
    val insert : 'a t -> priority -> 'a -> 'a t
    val remove : 'a t -> priority * 'a * 'a t
end;;

module PQueue : (PQUEUE with type priority = int) =
struct
    type priority = int
    type 'a t = ('a * priority) list

    exception EmptyPQueue

    let empty = []

    let insert l p a = (a, p)::l

    let rec getmax l = match l with
    | [] -> raise (EmptyPQueue)
    | [a] -> a
    | a::aa -> let mrest = getmax aa in 
            if (snd mrest) > (snd a)
            then
                mrest
            else
                a

    let rec myfilter needle l = match l with
        | [] -> []
        | x::xs ->
            if x = needle then xs
        else 
            x :: (myfilter needle xs)

    let remove l = 
        let top = getmax l in
            let nl = myfilter top l in
                (snd top), (fst top), nl

end;;

(* testing *)

let sort_ints (l : int list) = 
    let rec auxload todo cq = match todo with
    | [] -> cq
    | x::xs -> let nl = PQueue.insert cq (0-x) x in auxload xs nl;
in
    let rec auxret cq =
        try
            let p, a, nl = PQueue.remove cq in
                a :: (auxret nl)
        with
        | _ -> []
in
    auxret (auxload l (PQueue.empty));;

module type ORDTYPE = 
sig
    type t
    type comparison = LT | EQ | GT
    val compare : t -> t -> comparison

end;;


module OrdToPQueue (OrdType : ORDTYPE) : (PQUEUE with type priority = OrdType.t) =
struct
    open OrdType

    type priority = OrdType.t
    type 'a t = ('a * priority) list

    exception EmptyPQueue

    let empty = []

    let insert l p a = (a, p)::l

    let rec getmax l = match l with
    | [] -> raise (EmptyPQueue)
    | [a] -> a
    | a::aa -> let mrest = getmax aa in 
            if OrdType.compare (snd mrest) (snd a) = LT
            then
                mrest
            else
                a

    let rec myfilter needle l = match l with
        | [] -> []
        | x::xs ->
            if x = needle then xs
        else 
            x :: (myfilter needle xs)

    let remove l = 
        let top = getmax l in
            let nl = myfilter top l in
                (snd top), (fst top), nl

end;;

module CPQ = OrdToPQueue (struct
        type t = int
        type comparison = LT | EQ | GT
        let compare x y = if x = y then EQ else
        begin
            if x < y then LT else GT
        end
    end
)

let sort_ints2 (l : int list) = 
    let rec auxload todo cq = match todo with
    | [] -> cq
    | x::xs -> let nl = CPQ.insert cq x x in auxload xs nl;
in
    let rec auxret cq =
        try
            let p, a, nl = CPQ.remove cq in
                a :: (auxret nl)
        with
        | _ -> []
in
    auxret (auxload l (CPQ.empty));;


let sort (type s) (module Ordering : ORDTYPE with type t = s) l =
    let module MyQ = OrdToPQueue (Ordering) in
        let rec auxload todo cq = match todo with
        | [] -> cq
        | x::xs -> let nl = MyQ.insert cq x x in auxload xs nl;
    in
        let rec auxret cq =
            try
                let p, a, nl = MyQ.remove cq in
                    a :: (auxret nl)
            with
            | _ -> []
    in
    auxret (auxload l (MyQ.empty)) ;;

(* testing of sort *)

sort (module struct
    type t = char
    type comparison = LT | EQ | GT
    let compare a b = 
        let ca = Char.code a and cb = Char.code b in
            if ca = cb then EQ else begin
                if ca < cb then LT else GT
            end
end
) ['f'; 'n'; 'z'; 'd'; 'v'; 'b'; 's'; 'u'; 'o'; 'q'; 'j'; 'l'; 'k'; 'p'; 'm'; 'x'; 'e'; 'a'; 'a'; 't'; 'h'; 'w'; 'g'; 'r'; 'i'; 'y'; 'c'];;