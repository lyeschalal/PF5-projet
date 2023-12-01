open Regex_base


let rec repeat n w =(*ou utiliser cette fonction*)
  match n with 
  | 0 -> []
  | _ -> w @ repeat (n-1) w
           
let expr_repeat n e =
  let rec aux_repeat nbr expr = 
    match nbr with 
    | 0 -> Eps 
    | 1 -> expr
    | _ -> aux_repeat (nbr-1) (Concat(e,expr))
  in aux_repeat n e 
;;

let rec is_empty e =
  match e with 
  |Eps -> true
  |Concat(a,b) -> (is_empty a)&&(is_empty b)
  |Star(a)->is_empty a
  |Alt(a,b) -> (is_empty a)&&(is_empty b) (* pas sur de moi la !*)
  | _ -> false 

let rec null e =
  match e with 
  |Eps -> true
  |Concat(a,b) -> (null a)&&(null b)
  |Star(a)-> true
  |Alt(a,b) -> (null a)||(null b) 
  | _ -> false 
  

 let rec is_finite e =
   match e with 
    |Star a-> is_empty a
    |Concat(a,b)-> (is_finite a) && (is_finite b)
    |Alt(a,b) -> (is_finite a) && (is_finite b)
    |_ -> true

let product l1 l2 =
  let rec  aux l1' list = 
    match l1' with 
    |[]-> List.rev (sort_uniq list)
    |a::r ->  aux r ((List.map (fun x -> a@x  ) l2)@list)
  in aux l1  []

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
