open Regex_base

(**)
(*renvoie le mot w concaténé n fois avec lui-même*)
let rec repeat n w =
  match n with 
  | 0 -> [] 
  | _ -> w @ repeat (n-1) w
     
     
(*envoie une expression régulière qui reconnaît les mots
formés de la concaténation de n mots reconnus par e. *)          
let expr_repeat n e =
(*en utilisant une fonction auxiliare recursive terminale*)
  let rec aux_repeat nbr expr = 
    match nbr with 
    | 0 -> Eps    (*cas trivial*)
    | 1 -> expr    (*cas trivial*)
    | _ -> aux_repeat (nbr-1) (Concat(e,expr))
  in aux_repeat n e 
;;

(* Fonction pour vérifier si une expression régulière ne contient que le mot vide *)
let rec is_empty e =
  match e with 
  |Eps -> true  (*cas trivial*)
  |Concat(a,b) -> (is_empty a)&&(is_empty b)
  |Star(a)->is_empty a
  |Alt(a,b) -> (is_empty a)&&(is_empty b) 
  |Base a -> false (*l'expression ne contient pas de mot vide *)
  |Joker -> false 

let rec null e =
  match e with 
  |Eps -> true  (*cas trivial*)
  |Concat(a,b) -> (null a)&&(null b) (* si les deux expression reconnaissent le mot vide *)
  |Star(a)-> true    (*cas trivial*)
  |Alt(a,b) -> (null a)||(null b) (* si une des deux expression reconnait le mot vide *)
  |Base a -> false 
  |Joker -> false 
  

(* Fonction pour vérifier si e langage reconnu par "e" est fini*)
let rec is_finite e =
  match e with 
  |Star a-> is_empty a  (*vrai si seulement si star ne contient que le mot vide*)
  |Concat(a,b)-> (is_finite a) && (is_finite b)
  |Alt(a,b) -> (is_finite a) && (is_finite b)
  |Eps -> true (*cas trivial*)
  |Base a -> true (*cas trivial*)
  |Joker-> true  (*cas trivial*)
  
  
(*renvoie l’ensemble des mots formés de la concaténation
  d’un mot de l1 et d’un mot de l2*)
let product l1 l2 =
  let rec  aux l1' list = 
    match l1' with 
    |[]-> List.rev (sort_uniq list) (*pour inverser la liste*)
    |a::r ->  aux r ((List.map (fun x -> a@x  ) l2)@list) (*concatener l'encien liste avec la concatenation de l'element extrait de l1 et tout les element de l2*)
  in aux l1  []

(* Fonction pour convertir une liste de caractères en une liste de listes de caractères *)
  let char_list_to_list_of_lists chars =
    List.map (fun c -> [c]) chars
 
 (* Fonction pour concaténer deux listes d'options *)   
  let concat_option_lists lst1 lst2 =
     match (lst1, lst2) with
      | (Some l1, Some l2) -> Some (l1 @ l2)
      | (Some l, None) | (None, Some l) -> Some l
      | (None, None) -> None

let rec enumerate alphabet e =
  match is_finite e with 
  |false -> None
  |true -> 
      match e with 
      |Eps -> Some [[]]
      |Joker-> Some (char_list_to_list_of_lists alphabet )
      |Star(a)-> enumerate alphabet a
      |Base(a)-> Some [[a]]
      |Alt (a, b) -> concat_option_lists (enumerate alphabet a) (enumerate alphabet b)    
      |Concat(a,b) -> 
        let resultat1 = enumerate alphabet a in 
        let resultat2 = enumerate alphabet b  in 
        match (resultat1,resultat2)with
        |(Some r1 ,Some r2) -> Some (product r1 r2 )
        |(Some r1, None) | (None, Some r1) -> Some r1
        | (None, None) -> None
      
  
let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
