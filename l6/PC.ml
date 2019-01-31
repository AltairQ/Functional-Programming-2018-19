open Syntax

exception WPF of string ;;

let prop_conj_el (conj : 'a prop) =
  match conj with
  | Conj (a, b) -> a
  | _ -> raise (WPF "Cannot ConjEL (not a Conj)") ;;
  
let prop_conj_er (conj : 'a prop) =
  match conj with
  | Conj (a, b) -> b
  | _ -> raise (WPF "Cannot ConjER (not a Conj)") ;;

let prop_impl_hd (impl : 'a prop) =
  match impl with
  | Impl (a, b) -> a
  | _ -> raise (WPF "Cannot extract prop. from Impl (not a Impl)") ;;

let prop_impl_tl (impl : 'a prop) =
  match impl with
  | Impl (a, b) -> b
  | _ -> raise (WPF "Cannot extract proof from Impl (not a Impl)") ;;

let prop_disj_extr (disj : 'a prop) =
  match disj with
  | Disj (a, b) -> a,b
  | _ -> raise (WPF "Cannot unpack Disj (not a Disj)") ;;

let hypt_get_prop (hyp : 'a hypt) =
  let propo, _ = hyp in propo ;;

let hypt_get_proof (hyp: 'a hypt) =
  let _, prf = hyp in prf ;;

let rec verify_pt (proof : string pt) = match proof with
  | Ax a -> (a, [a])
  | TopI -> (Top, [])
  | ConjI (a, b) ->
          let (z1, r1) = verify_pt a in
          let (z2, r2) = verify_pt b in
            (Conj (z1, z2), r1@r2)
  | ImplI (presum, prf) ->
          let (z, r) = verify_pt prf in
            let nr = List.filter (fun x -> x <> presum) r in
              (Impl (presum, z), nr)
  | BotE prop -> (prop, [Bot])
  | ConjEL a ->
          let (z, r) = verify_pt a in
            (prop_conj_el z), r
  | ConjER a ->
          let (z, r) = verify_pt a in
            (prop_conj_er z), r
  | ImplE (a, b) ->
          let (z1, r1) = verify_pt a in
          let (z2, r2) = verify_pt b in
            if (prop_impl_hd z2) = z1 then
              (prop_impl_tl z2), (r1@r2)
            else
              raise (WPF "ImplE error (not a proof of Impl or different Ps)")
  | DisjIL (prof, propo) ->
          let (z, r) = verify_pt prof in
            (Disj (z, propo)), r
  | DisjIR (propo, prof) ->
          let (z, r) = verify_pt prof in
            (Disj (propo, z)), r
  | DisjE (prof, hyp1, hyp2) -> 
          let (ds, rds) = verify_pt prof in
          let p, q = prop_disj_extr ds in
            if (hypt_get_prop hyp1) = p && (hypt_get_prop hyp2) = q then
              let (z1, r1) = verify_hyp_pt p (hypt_get_proof hyp1) in
              let (z2, r2) = verify_hyp_pt q (hypt_get_proof hyp2) in
                if z1 = z2 then
                  z1, (rds@r1@r2)
                else
                  raise (WPF "DisjE error (different Rs)")
            else
              raise (WPF "DisjE error (wrong P or Q)")
and 
  verify_hyp_pt (presum : 'a prop) (prof : 'a pt) = 
  let (z, r) = verify_pt prof in
    let nr = List.filter (fun x -> x <> presum) r in
      (z, nr)
;;

(* let bruteforce_ps (axioms : 'a prop list) (cgoal : 'a prop list) = match cgoal with
	| Top -> Top
	| sf -> expr2


let verify_ps (subgoals : 'a prop list) (proof : 'a ps) = match proof with
  | PDone a-> a
  | _ -> expr2 *)


let vps_wrap (pr : 'a prop) (proof : 'a ps) =
	print_string "ignored for now.";;

let vpt_wrap (pr : 'a prop) (prf : 'a pt) =
  try
    let (z, r) = verify_pt prf in
      if z = pr then
        if r = [] then
          print_string "correct."
        else 
          print_string "wrong - free axiom(s)!"
      else
        print_string "wrong - proof differs from proposition!"
  with
  | (WPF s) -> print_string ("proof error - "^s);;

  

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let proofs = Parser.file Lexer.token lexbuf
  in let rec switch_verify = (function
  | TGoal (s, prop, pt) -> print_string ("[t] "^s^": "); vpt_wrap prop pt |> ignore; print_string "\n"
  | SGoal (s, prop, ps) -> print_string ("[s] "^s^": "); vps_wrap prop ps |> ignore; print_string "\n"
)
in
  	List.iter (switch_verify) proofs ;;
