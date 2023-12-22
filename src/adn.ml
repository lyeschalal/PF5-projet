type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
  | A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  List.init (String.length str) (fun indice -> str.[indice]);;
 (* List.init var1 var2  permet de creer une list de taille var1 en appliquant 
    la fonction donner en var2 pour chaque element de la liste  *)

(* conversions *)
(* Fonction pour convertir un caractère en base *)
let base_of_char (c : char) : base =
  match c with
   | 'A' -> A
   | 'C' -> C
   | 'G' -> G
   | 'T' -> T
   | _  -> WC (*les autre cas*)

(* convertir string en DNA    *)
(* Fonction pour convertir une chaîne de caractères en séquence d'ADN *)
let dna_of_string (s : string) : base list =
  List.map (base_of_char) (explode s)


(* convertir DNA en string   *)
(* Fonction pour convertir une séquence d'ADN en chaîne de caractères *)
let string_of_dna (seq : dna) : string =
  String.concat ""  (List.map (string_of_base) (seq));;



(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec  cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  match ( slice , list ) with
  |( _::_ ,[] ) -> None
  |( [] , r2 ) -> Some r2 
  |( a::r1 , b::r2 ) -> if a != b then None else cut_prefix r1 r2 ;;


(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
(*
   first_occ slice list recherche la première occurrence de la séquence slice
   dans la liste, et retourne le préfixe et le suffixe de la liste avant et après
   cette occurrence. Si aucune occurrence n'est trouvée, la fonction retourne None.

   - Paramètres :
     - slice : La séquence à rechercher.
     - list : La liste dans laquelle rechercher l'occurrence.

   - Retour :
     Une option contenant le préfixe et le suffixe de la liste avant et après
     la première occurrence de slice, ou None si aucune occurrence n'est trouvée.
*)

  let first_occ (slice : 'a list) (list : 'a list): ('a list * 'a list) option =
   let rec aux_first_occ before liste =
    match cut_prefix slice liste with 
    |Some after -> Some (List.rev before,after)
    | None ->
        match liste with
        | [] -> None
        | hd::reste -> aux_first_occ (hd::before) reste
  in aux_first_occ [] list
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)



 (*
    slices_between start stop list recherche des occurrences de la séquence start
    et stop dans la liste, puis extrait les tranches de la liste entre ces occurrences.
 
    - Paramètres :
      - start : La séquence de début à rechercher.
      - stop : La séquence de fin à rechercher.
      - list : La liste dans laquelle rechercher les occurrences.
 
    - Retour :
      Une liste de tranches de la liste, chaque tranche étant entre une occurrence
      de start et stop. Si aucune occurrence n'est trouvée, la liste résultante est vide.
 *)

  let slices_between (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =       
    let rec aux acc list =
      (* Recherche de la première occurrence de la séquence 'start' dans la liste *)
       match first_occ start list with
        | None -> List.rev acc (* Aucune occurrence trouvée, renvoie l'accumulateur inversé *)
        | Some (pre, suf) ->
           (* Recherche de la première occurrence de la séquence 'stop' dans la suite 'suf' *)
            match first_occ stop suf with
            | None -> List.rev acc (* Aucune occurrence trouvée, renvoie l'accumulateur inversé *)
            | Some (pre_2, suf_2) -> aux (pre_2 :: acc) suf_2 
              (* Une occurrence de 'stop' est trouvée, ajoute la portion entre 'start' et 'stop'
               à l'accumulateur et appelle récursivement avec la suite 'suf_2' *)
        in aux [] list    

 (*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

 let cut_genes (dna : dna) : (dna list) =
  slices_between [A; T; G] [T; A; A] dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
 (*
   consensus list calcule le consensus d'une liste d'éléments.

   - Paramètre :
     - list : La liste d'éléments pour laquelle calculer le consensus.

   - Retour :
     Un variant du type 'a consensus représentant le consensus de la liste.
     - Full a : Si tous les éléments de la liste sont égaux à a.
     - Partial (a, n) : Si a est l'unique valeur apparaissant le plus grand nombre
       de fois dans list et si a apparaît exactement n fois dans list.
     - No_consensus : Dans tous les autres cas, lorsque la liste est vide ou
       s'il n'y a pas de consensus clair.

   Remarque : La fonction utilise la fonction auxiliaire remove_from_left pour
   enlever les éléments répétés dans la liste avant de calculer le consensus.Et aussi la fonction 
   nb_occ_element pour compter le nombre d'occurence d'un element dans la liste .
*)

(*Retourne le nombre d'occurence d'un element dans la liste*)
let rec nb_occ_element (list : 'a list) (element : 'a ) : int = 
  List.fold_left (fun acc x -> if x = element then acc + 1 else acc) 0 list

(* Retourne une nouvelle liste où seuls les premiers occurrences de chaque élément sont conservés,
  dans l'ordre d'apparition*)  
let remove_from_left (xs : 'a list) : 'a list =
  List.rev (List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] xs)

let consensus (list : 'a list) : 'a consensus =
    (* Fonction auxiliaire récursive qui prend une liste sans doublants 'list_tmp' pour trouver
        l'élément avec le plus grand nombre d'occurrences dans 'list' *)
    let rec max_occ_list list_tmp =
      match list_tmp with
      | [] -> raise Not_found
      | [a] -> (nb_occ_element list a, a)
      | a :: r ->
          (*on compte le nombre d'occurence de 'a' dans 'list' *)
          let occ_tmp = nb_occ_element list a in
          (*on cherche le nombre avec la plus grande occurence dans le reste 'r' *)
          let occ_tmp2 = max_occ_list r in
          match occ_tmp2 with
          (*on compare 'occ_tmp' et 'occ_tmp2' et on retourne le plus grand avec son occurence, 
            dans le cas d'égalité on retourne l'exception 'Not_found' *)
          | (nb, element) ->
              if occ_tmp > nb then (occ_tmp, a) 
              else if occ_tmp = nb then raise Not_found
              else (nb, element)
    in
    try
      (*on fait appel à max_occ_list sur la liste 'list' sans doublants *)
      match max_occ_list (remove_from_left list) with
      | (nb, element) when nb = List.length list -> Full element
      | (nb, element) -> Partial (element, nb)
    with Not_found -> No_consensus

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
  with Not_found -> No_conse
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)
 (*
   consensus_sequence ll calcule le consensus de chaque position dans une liste
   de séquences.

   - Paramètre :
     - ll : Une liste de séquences.

   - Retour :
     Une liste de variantes du type 'a consensus représentant les consensus
     de chaque position dans les séquences. Si toutes les listes sont vides,
     la fonction retourne une liste vide.
     *)

  let consensus_sequence (ll : 'a list list) : 'a consensus list  =
  (* Fonction auxiliaire récursive *)
  let rec aux_consensus_sequence acc l =
    match l with
    | [] -> List.rev acc  (* Toutes les listes sont traitées, retourne l'accumulateur inversé *)
    | _ -> 
        (* Extraction de la colonne actuelle et du reste des séquences *)
        let colonne = List.map (fun sous_liste -> List.hd sous_liste) l in 
        let reste = List.map (fun sous_liste -> List.tl sous_liste) l in
        (* Vérification si toutes les séquences sont vides *)
        match List.mem [] reste with
        | true -> aux_consensus_sequence (consensus colonne :: acc) []  (* Toutes vides, ajoute le dernier consensus  *)
        | false -> aux_consensus_sequence (consensus colonne :: acc) reste  (* Ajoute le consensus et passe à la colonne *)
  in aux_consensus_sequence [] ll


(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
