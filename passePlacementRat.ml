module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct
  open Type
  open Exceptions
  open Ast
  open AstPlacement
  open Tds

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme

  let get_type ia =
    let InfoVar(n,t,_,_) = info_ast_to_info ia in t

  let rec analyser_instruction reg dep i =
    match i with
    |AstType.Declaration(ia,e)-> let t = getTaille(get_type ia) in
      (modifier_adresse_info dep reg ia) ;
      (i, t+dep)
    |AstType.TantQue(e,b)-> let _= analyser_bloc reg dep b in
      (i,dep)
    |AstType.Conditionnelle(e,bt,be)-> let _=analyser_bloc reg dep bt in 
      let _ =analyser_bloc reg dep be in 
      (i,dep)
    |_ -> (i,dep)


  and analyser_bloc reg dep li=
    match li with
    |[]->[]
    |i::q-> let (ni,ndep)=analyser_instruction reg dep i in 
      ni::(analyser_bloc reg ndep q)

let rec analyser_parametre dep rlp =
  match rlp with
  |[] -> []
  |ia::q->let t=getTaille(get_type ia) in 
    modifier_adresse_info (dep-t) "LB" ia;
    ia::analyser_parametre (dep-t) q 

let analyser_fonction (AstType.Fonction(ia,lp,b)) =
  let nb = analyser_bloc "LB" 3 b in 
  let nlpi = analyser_parametre 0 (List.rev lp) in 
  Fonction(ia,nlpi,nb)
   



let analyser (AstType.Programme(lf,b)) =
  let nlf= List.map analyser_fonction lf in 
  let nb=analyser_bloc "SB" 0 b in 
  Programme(nlf,nb)
end