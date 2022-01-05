module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct
  open Type
  open Exceptions
  open Ast
  open AstPlacement
  open Tds

  type t1 = Ast.AstPlacement.programme
  type t2 = string

(* Génération d'étiquette à l'aide d'un compteur *)
let getEtiquette = 
  let num = ref 0 in
  fun () ->
    num := (!num)+1 ;
    "label"^((string_of_int (!num)))

let pgcd = 
"pgcd
LOADL 0
LOAD (1) -2[LB]
LOAD (1) -1[LB]
boucle
LOAD (1) 5[LB]
JUMPIF (0) fin
LOAD (1) 4[LB]
LOAD (1) 5 [LB]
SUBR IMod
STORE (1) 3[LB]
LOAD (1) 5[LB]
STORE (1) 4[LB]
LOAD (1) 3[LB]
STORE(1) 5[LB]
JUMP boucle
fin
LOAD (1) 4[LB]
RETURN (1) 2\n\n"

let norm =
"norm
LOAD (1) -2[LB]
LOAD (1) -1[LB]
CALL (LB) pgcd
LOAD (1) -2[LB]
LOAD (1) 3[LB]
SUBR IDiv
LOAD (1) -1[LB]
LOAD (1) 3[LB]
SUBR IDiv
RETURN (2) 2\n\n"

let rout =
"ROut
LOADL '['
SUBR COut
LOAD (1) -2[LB]
SUBR IOut
LOADL '/'
SUBR COut
LOAD (1) -1[LB]
SUBR IOut
LOADL ']'
SUBR COut
RETURN (0) 2\n\n"

let radd =
"RAdd
LOAD (1) -4[LB]
LOAD (1) -1[LB]
SUBR IMul
LOAD (1) -2[LB]
LOAD (1) -3[LB]
SUBR IMul
SUBR IAdd
LOAD (1) -3[LB]
LOAD (1) -1[LB]
SUBR IMul
CALL (ST) norm
RETURN (2) 4\n\n"

let rmul =
"RMul
LOAD (1) -4[LB]
LOAD (1) -2[LB]
SUBR IMul
LOAD (1) -3[LB]
LOAD (1) -1[LB]
SUBR IMul
CALL (ST) norm
RETURN (2) 4\n\n"

(* Entête des fichiers Rat  contenant :
- un saut vers le programme principal
- la fonction pgcd nécessaire à la normalisation des rationnels
- une fonction de normalisation des rationnels
- les fonctions d'affichage (ROut), d'addition (RAdd) et de multiplication (RMult) de rationnel
*)
let getEntete () =
  "JUMP main\n\n"
  ^pgcd
  ^norm
  ^rout
  ^radd
  ^rmul

(*Ecriture dans un fichier *)
let ecrireFichier nom texte =
  let fich = open_out nom in
  output_string fich texte ;
  close_out fich
let rec fusion l1 l2=
  match l1,l2 with 
  | [],[] -> [] 
  | d1::q1, d2::q2 -> (d1,d2)::(fusion q1 q2)

  let get_type ia =
    let InfoVar(n,t,_,_) = info_ast_to_info ia in t

  let analyse_unaire u =
    if (u=AstType.Numerateur) then
      "POP (0) 1 \n"
    else
      "POP (1) 1 \n"
  
  let analyse_binaire b =
   match b with
    |AstType.PlusInt -> ("SUBR IAdd \n",Int)
    |AstType.PlusRat -> ("CALL (SB) RAdd \n",Rat)
    |AstType.MultInt ->("SUBR IMul \n",Int)
    |AstType.MultRat ->("CALL (SB) RMul \n",Rat)
    |AstType.EquInt ->("SUBR IEq \n",Int)
    |AstType.EquBool ->("SUBR IEq \n",Bool)
    |AstType.Inf->("SUBR ILss \n",Int)
    |AstType.Fraction-> ("",Int)

  let rec analyse_expression (e,t) =
    match e with
    |AstType.Booleen(b) ->
        if b then "LOADL 1 \n" else "LOADL 0 \n"
    |AstType.Ident(ia)-> let InfoVar(_,_,add,reg) = info_ast_to_info ia in
    let ta = (Type.getTaille t) in 
        "LOAD ("^(string_of_int ta)^") "^string_of_int add^"["^reg^"] \n"
    |AstType.Entier(i) ->  
    "LOADL "^(string_of_int i)^"\n"
    |AstType.Unaire(u,e1) -> let code1=analyse_expression (e1,Rat) in
    let code2=analyse_unaire(u) in
      code1^code2
    |AstType.Binaire(b,e1,e2) ->
      let (codeb,t1) = analyse_binaire b in
      let code1=analyse_expression(e1,t1) in
      let code2=analyse_expression(e2,t1) in
      code1^code2^codeb
   |AstType.AppelFonction(ia,la) ->
    let InfoFun(nom,t_ret,l_type) = info_ast_to_info ia in
    let lst= (List.map analyse_expression (fusion la l_type)) in
    let nla =(List.fold_right (fun x y -> x^y) lst "") in
    nla^"CALL (SB) "^nom^"\n" 

  let rec analyse_instruction tr tp i  =
  match i with 
  |AstType.Declaration(ia,e) -> let InfoVar(_,t,add,reg) = info_ast_to_info ia in
    let codee=analyse_expression (e,t) in
    let taille=string_of_int (getTaille t) in
    ("PUSH ("^taille^") \n"^codee^"STORE ("^taille^") "^(string_of_int add)^"["^reg^"] \n",(getTaille t))
  |AstType.Affectation(ia,e) -> let InfoVar(_,t,add,reg) = info_ast_to_info ia in
    let codee=analyse_expression (e,t) in
    let taille=string_of_int (getTaille t) in
    (codee^"STORE ("^taille^") "^(string_of_int add)^"["^reg^"] \n",0)
  |AstType.AffichageInt(e) -> let codee=analyse_expression (e,Int) in
    (codee^"SUBR IOut \n",0)
  |AstType.AffichageRat(e) -> let codee=analyse_expression (e,Rat) in
    (codee^"CALL (SB) ROut",0)
  |AstType.AffichageBool(e) -> let codee=analyse_expression (e,Bool) in
    (codee^"SUBR BOut \n",0)
  |AstType.Conditionnelle(e,bt,be) ->
    let bloc_else=(getEtiquette()) in
    let fin_else=(getEtiquette()) in
    let codee=analyse_expression (e,Bool) in
    let codebt=analyse_bloc tr tp bt in
    let codebe=analyse_bloc tr tp be in
    (codee^"JUMPIF (0) "^bloc_else^"\n"^codebt^"JUMP "^fin_else^"\n"^bloc_else^"\n"^codebe^fin_else^"\n",0)
  |AstType.Retour(e) -> let codee=analyse_expression(e,tr) in
  let ttr=getTaille tr in
    (codee^"RETURN ("^(string_of_int ttr)^") "^(string_of_int tp)^"\n",0)
  |AstType.TantQue(e,bl) -> let codee=analyse_expression(e,tr) in 
  let bloc_while=(getEtiquette ()) in
  let fin_while=(getEtiquette() ) in
  let codebl=analyse_bloc tr tp bl in
  (bloc_while^"/n"^codee^"JUMPIF (0) "^fin_while^"\n"^codebl^"JUMP "^bloc_while^"\n"^fin_while^"\n",0)
  |AstType.Empty ->("",0)

  and analyse_bloc tr tp bl = let nbl= List.map (analyse_instruction tr tp) bl in
    let taille_pop=List.fold_left (fun a b ->  a+ (snd b) ) 0 nbl in
    let pop=string_of_int taille_pop in
    let codeb=List.fold_left (fun a b -> a^(fst b)) ("") nbl in
    codeb^"POP (0) "^pop^"\n"

  

  let analyser (Programme(lf,bl))=
  let debu= (getEntete() ) in
  let nbl=analyse_bloc Undefined 0 bl in 
  (*let nlf=List.fold_left (fun x y -> x^y) "" (List.map (analyse_fonction) lf) in*)
    debu^"main \n"^nbl^"HALT \n" 

end
