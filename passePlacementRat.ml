module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct
  open Type
  open Exceptions
  open Ast
  open AstPlacement
  open Tds

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme

(* get_type : info_ast -> typ *)
(* Paramètre info_ast : l'info_ast du quelle on veut extraire le type*)
(* Renvoie le type d'une InfoVar ou d'une InfoConst *)
(* Erreur si les types si elle est mal utilisée  *)
   let get_type ia =
    match info_ast_to_info ia with
    | InfoVar(_,t,_,_) ->  t
    | InfoConst _ -> Int
    | _ -> failwith ""

(* analyser_instruction : string -> int ->instruction  *)
(* Paramètre reg : le registre*)
(* Paramètre dep : le déplacement*)
(* Paramètre i : l'instruction'*)
(* Renvoie l'instruction avec le déplacement à faire en prenant en considération le déplacement initial *)
  let rec analyser_instruction reg dep i =
    match i with
    |AstType.Declaration(ia,_)-> let t = getTaille(get_type ia) in
      (modifier_adresse_info dep reg ia) ;
      (i, t+dep)
    |AstType.TantQue(_,b)-> let _= analyser_bloc reg dep b in
      (i,dep)
    |AstType.Conditionnelle(_,bt,be)-> let _=analyser_bloc reg dep bt in 
      let _ =analyser_bloc reg dep be in 
      (i,dep)
    |_ -> (i,dep)

(* analyser_bloc : string -> int ->bloc *)
(* Paramètre reg : le registre*)
(* Paramètre dep : le déplacement*)
(* Paramètre i : le bloc*)
(* Renvoie la liste des instructions du bloc en prenant en consideration le déplacement fait entre une instruction et une autre  *)
  and analyser_bloc reg dep li=
    match li with
    |[]->[]
    |i::q-> let (ni,ndep)=analyser_instruction reg dep i in 
      ni::(analyser_bloc reg ndep q)

(* analyser_parametre : int -> info_ast list *)
(* Paramètre dep : le déplacement*)
(* Paramètre i : le bloc*)
(* modifie l'adresse de l'info des parametres de la fonction  *)
let rec analyser_parametre dep rlp =
  match rlp with
  |[] -> []
  |ia::q->let t=getTaille(get_type ia) in 
    modifier_adresse_info (dep-t) "LB" ia;
    ia::analyser_parametre (dep-t) q 

(* analyser_fonction : AstType.Fonction -> Fonction *)
(* Paramètre Fonction : la fonction à analyser *)
(* Initialise le registre LB pour le bloc des instructions de la fonction et met les parametres dans leurs adresses  *)
let analyser_fonction (AstType.Fonction(ia,lp,b)) =
  let nb = analyser_bloc "LB" 3 b in 
  let nlpi = analyser_parametre 0 (List.rev lp) in 
  Fonction(ia,nlpi,nb)
   
(* analyser : AstType.Programme -> Programme *)
(* Paramètre Programme : le programme à analyser *)
(* Initialise le registre SB pour le bloc du programme et appelle analyse_fonction sur les fonctions du programme  *)
let analyser (AstType.Programme(lf,b)) =
  let nlf= List.map analyser_fonction lf in 
  let nb=analyser_bloc "SB" 0 b in 
  Programme(nlf,nb)
end