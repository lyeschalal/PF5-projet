open Regex_base

(*renvoie le mot w concaténé n fois avec lui-même*)
 
  let repeat n w =
    let rec aux acc nbr =
      match nbr with
      | 0 ->  acc
      | _ -> aux (w @ acc) (nbr - 1)
    in
    aux [] n
   
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

let  is_empty e = 
  let rec aux l =
   match l with 
     |[] -> true (*cas trivial*)
     |Eps::r -> aux r
     |Concat(a,b)::r -> aux(a::b::r)
     |Star(a)::r-> aux(a::r)
     |Alt(a,b)::r-> aux(a::b::r)
     |Base(a)::r -> false (*l'expression ne contient pas de mot vide *)
     |Joker::r -> false 
  in aux [e]  

let rec null e =
  match e with 
  |Eps -> true  (*cas trivial*)
  |Concat(a,b) -> (null a)&&(null b) (* si les deux expression reconnaissent le mot vide *)
  |Star(a)-> true    (*cas trivial*)
  |Alt(a,b) -> (null a)||(null b) (* si une des deux expression reconnait le mot vide *)
  |Base a -> false 
  |Joker -> false 
  

(* Fonction pour vérifier si e langage reconnu par "e" est fini*)
let is_finite e =
 let rec aux l =
  match l with 
  |[]-> true
  |Star(a)::r-> is_empty a (*vrai si seulement si star ne contient que le mot vide*)
  |Concat(a,b)::r-> aux(a::b::r)
  |Alt(a,b)::r -> aux(a::b::r)
  |Eps::r -> aux r (*cas trivial*)
  |Base(a)::r -> aux r (*cas trivial*)
  |Joker::r-> aux r  (*cas trivial*)  
  in aux [e]  
  
  
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
      
  
      
      
  
(*renvoie l’ensemble des lettres apparaissant dans e*)
let alphabet_expr e =
  let rec aux e liste=
  match e with 
  |Base a -> sort_uniq (a::liste)
  |Concat(a,b)-> aux a (aux b liste) 
  |Alt(a,b)-> aux a (aux b liste) 
  |Star a-> aux a liste 
  |Eps -> sort_uniq liste
  |Joker-> sort_uniq liste
in aux e []



type answer =
  Infinite | Accept | Reject


(*renvoie  – Infinite si le langage reconnu par e est infini,
           – Accept si le langage reconnu par e est fini et contient le mot w,
           – Reject si le langage reconnu par e est fini et ne contient pas w.*)

let accept_partial e w =
  match is_finite e with 
  |false ->Infinite  (* langage infini *)
  |true -> (
    let resultat = enumerate w e in 
      match resultat with
        |Some res ->( 
              match List.mem w res  with
              |true -> Accept (*fini et contient le mot w *)
              |false ->Reject (*fini et ne contient pas le mot w *)
              )
       |None -> Reject       (*fini et ne contient pas le mot w *)
  )
