type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
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
let base_of_char (c : char) : base =
  match c with
   | 'A' -> A
   | 'C' -> C
   | 'G' -> G
   | 'T' -> T
   | _  -> WC (*les autre cas*)

(* convertir string en DNA    *)
let dna_of_string (s : string) : base list =
  (* convertir chaque element de la liste retourner par explode en base *)
  List.map (base_of_char) (explode s)


(* convertir DNA en string   *)
let string_of_dna (seq : dna) : string =
  (* d'abord convertir ADN en liste de string    *)
  (* ensuite la liste obtenue avant en string  *)
  String.concat ""  (List.map (string_of_base) (seq));;



(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix pre l =
  match ( pre , l ) with
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
let rec first_occ slice l = 
  let rec aux_first_occ before liste =
    match cut_prefix slice liste with 
    |Some after -> Some (List.rev before,after)
    | None ->
        match liste with
        | [] -> None
        | hd::reste -> aux_first_occ (hd::before) reste
  in aux_first_occ [] l 
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


 let rec slices_between
 (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
match first_occ start list with 
|None -> []
|Some (pre,suf) -> match first_occ stop suf with 
            |None -> []
            |Some (pre_2 , suf_2) -> pre_2::(slices_between start stop suf_2)


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
let rec nb_occ_element (list : 'a list) (element : 'a ) : int =
    match list with 
    |[]-> 0
    |a::r when a=element-> 1+  nb_occ_element r element
    |a::r -> nb_occ_element r element


let cons_uniq xs x = if List.mem x xs then xs else x :: xs

let remove_from_left xs = List.rev (List.fold_left cons_uniq [] xs)

exception Pas_de_max_occ ;;
let consensus (list : 'a list) : 'a consensus =
  let rec max_occ_liste  (list_tmp : 'a list)  : int * 'a  =
    match list_tmp with 
    |[]-> raise (Pas_de_max_occ )
    |[a]-> (nb_occ_element list a,a)
    |a::r ->let occ_tmp = nb_occ_element list a and occ_tmp2 = max_occ_liste r
        in   match  occ_tmp2  with 
        |(nb,element ) -> match occ_tmp > nb with 
          |true -> (occ_tmp,a)
          |false when occ_tmp = nb -> raise (Pas_de_max_occ ) 
          | _ -> (nb,element )
              
  in try ( match max_occ_liste (remove_from_left list)   with
      |(0,_) ->  No_consensus
      |(nb,element)when nb = List.length list -> Full element
      |(nb,element )-> Partial (element ,nb)) 
  with 
  |Pas_de_max_occ-> No_consensus

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  failwith "À compléter"

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
