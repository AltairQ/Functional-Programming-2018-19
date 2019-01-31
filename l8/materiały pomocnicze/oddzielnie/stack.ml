  type 'a t = { mutable l : 'a list }
  exception Empty of string

  let create()= { l = [] }
  let push(e,s) = s.l <- e :: s.l

  let top s =
    match s.l with
      hd::_ -> hd
    | []     -> raise (Empty "module Stack: top")

  let pop s =
    match s.l with
      hd::tl -> s.l <- tl
    | []     -> ()

  let isEmpty s = s.l = []


