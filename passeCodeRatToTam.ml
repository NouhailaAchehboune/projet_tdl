module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct
  open Type
  open Exceptions
  open Ast
  open AstPlacement
  open Tds
  open Code

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
STORE (1) 5[LB]
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

(* fusion : 'a list -> ('a * 'a) list *)
(* Paramètre l1 : Une première liste *)
(* Paramètre l2 : Une deuxième liste *)
(* Fusionne les deux listes en une liste de couples *)
let rec fusion l1 l2=
  match l1,l2 with 
  | [],[] -> [] 
  | d1::q1, d2::q2 -> (d1,d2)::(fusion q1 q2)

(* get_type : info_ast -> typ *)
(* Paramètre ia : Une info_ast d'une variable *)
(* Renvoie le type récupéré de l'info_ast *)
(* Erreur si mauvaise utilisation de la fonction *)
  let get_type ia =
    let InfoVar(n,t,_,_) = info_ast_to_info ia in t

(* analyse_unaire : unaire -> string *)
(* Paramètre L'unaire à traiter*)
(* Renvoie le code tam nécessaire si on veut récuperer un unaire en sommet de pile  *)
  let analyse_unaire u =
    if (u=AstType.Numerateur) then
      "POP (0) 1 \n"
    else
      "POP (1) 1 \n"

(* analyse_binaire : binaire -> string *)
(* Paramètre b : le binaire à traiter *)
(* Renvoie le code tam de l'appelle de fonction qui réalise le binaire *)
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

(* position_var : (typ * string)list -> int *)
(* Paramètre Enre(le) : l'enregistrement à traiter *)
(* Paramètre id : l'id du champ dont on veut récuperer la position *)
(* Renvoie lla position de l'id dans l'enregistrement *)
(* Erreur si mauvaise utilisation de la fonction *)
    let rec position_var (Enre(le)) id =
        match le with
        | (t,nom)::q ->if (id=nom) then 0
                        else (getTaille t)+(position_var (Enre(q)) id)
        | [] -> failwith ""  

(* analyse_affectable_gauche :  affectable-> string *)
(* Paramètre a : l'affectable se trouvant à gauche dans le code *)
(* Renvoie le code tam pour l'affectable qui se trouve à gauche *)
(* Erreur si mauvaise utilisation de la fonction *)
   let rec analyse_affectable_gauche a=
    match a with
    |AstType.Ident(ia)-> let InfoVar(_,t,add,reg) = info_ast_to_info ia in
        ("LOADA "^string_of_int add^"["^reg^"] \n",t)
    |AstType.Deref(a1) -> let (codea,ta)=analyse_affectable_gauche a1 in
                            begin
                            match ta with
                            |Pointeur(t)-> let taille = getTaille t in
                                  (codea^"LOADI ("^(string_of_int taille)^") \n",t)
                            |_ -> failwith ""
                            end 
    |AstType.Acces(a1,ia)-> let AstType.Ident(ia)=a1 in
                            let InfoVar(_,t,add,reg)=info_ast_to_info ia in
                            let InfoVar(nom,_,_,_)=info_ast_to_info ia in
                            let pos=position_var t nom in
                            ("LOADA "^string_of_int (add+pos)^"["^reg^"] \n",Undefined)
                            
(* analyse_affectable :  affectable -> typ-> string *)
(* Paramètre a : l'affectable se trouvant à droite dans le code *)
(*Paramètre t : le type de l'affectable *)
(* Renvoie le code tam pour l'affectable qui se trouve à gauche *)
(* Erreur si mauvaise utilisation de la fonction *)   
    let rec analyse_affectable (a,t)=
    match a with
    |AstType.Ident(ia)-> begin 
                          match info_ast_to_info ia with 
                          |InfoVar(_,_,add,reg) -> let ta = (getTaille t) in 
                                                    "LOAD ("^(string_of_int ta)^") "^string_of_int add^"["^reg^"] \n"
                          |InfoConst(_,i)-> "LOADL "^(string_of_int i)^" \n"
                          |_ -> failwith ""
                        end
    |AstType.Deref(a1) -> let na= analyse_affectable (a1,t) in
                          na^"LOADI ("^(string_of_int(getTaille t))^") \n"
                          (*On appelle analyse_affectable_gauche car on a pas acces au type de a1*)
    | AstType.Acces(a1,ia) -> let (na,t) = analyse_affectable_gauche (a1) in
                              let InfoVar(nom,t_ia,_,_) = info_ast_to_info ia in
                              let pos = position_var t nom in
                              let taille=getTaille t_ia in 
                              let pos1=pos+taille in
                              na^"LOADI ("^(string_of_int(pos+taille))^") \n"^"POP ("^(string_of_int taille)^") "^(string_of_int pos)^" \n"
  
   

  let rec analyse_expression (e,t) =
    match e with
    |AstType.Booleen(b) ->
        if b then "LOADL 1 \n" else "LOADL 0 \n"
    |AstType.Affectable(a)-> let codea=analyse_affectable (a,t) in
        codea
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
    |AstType.Null -> ""
    |AstType.New(t) -> let ta=getTaille t in
    "LOADL "^(string_of_int ta)^ " \n"^"SUBR Malloc \n"
    |AstType.Adresse(ia)-> begin
                            match info_ast_to_info ia with
                            | InfoVar(_, t, reg, dep) -> "LOADA ("^(string_of_int(getTaille t))^") "^(string_of_int reg)^"["^dep^"]\n"
                            | _ -> failwith ""
                          end
    |AstType.Creation(le) ->let Enre(lt)=t in
        let lst= (List.map analyse_expression (fusion le (List.map fst lt))) in
        let nla =(List.fold_right (fun x y -> x^y) lst "") in
                              nla
  let rec analyse_instruction tr tp i  =
  match i with 
  |AstType.Declaration(ia,e) -> let InfoVar(_,t,add,reg) = info_ast_to_info ia in
    let codee=analyse_expression (e,t) in
    let taille=string_of_int (getTaille t) in
    ("PUSH "^taille^" \n"^codee^"STORE ("^taille^") "^(string_of_int add)^"["^reg^"] \n",(getTaille t))
  |AstType.Affectation(a,e) -> let (codea,t) = analyse_affectable_gauche a in
    let codee=analyse_expression (e,t) in
    let taille=string_of_int (getTaille t) in
    (codee^codea^"STOREI ("^taille^") \n",0)
  |AstType.AffichageInt(e) -> let codee=analyse_expression (e,Int) in
    (codee^"SUBR IOut \n",0)
  |AstType.AffichageRat(e) -> let codee=analyse_expression (e,Rat) in
    (codee^"CALL (SB) ROut \n",0)
  |AstType.AjoutInt(ia,e) -> let InfoVar(_,_,add,reg) = info_ast_to_info ia in
    let codee=analyse_expression (e,Int) in
        ("LOAD (1) "^string_of_int add^"["^reg^"] \n"^codee^"SUBR IAdd \n"^"STORE (1) "^(string_of_int add)^"["^reg^"] \n",0)
  |AstType.AjoutRat(ia,e) -> let InfoVar(_,_,add,reg) = info_ast_to_info ia in
    let codee=analyse_expression (e,Rat) in
        ("LOAD (2) "^string_of_int add^"["^reg^"] \n"^codee^"CALL (SB) RAdd \n"^"STORE (2) "^(string_of_int add)^"["^reg^"] \n",0)
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
  (bloc_while^" \n"^codee^"JUMPIF (0) "^fin_while^" \n"^codebl^"JUMP "^bloc_while^" \n"^fin_while^" \n",0)
  |AstType.Empty ->("",0)

  and analyse_bloc tr tp bl = let nbl= List.map (analyse_instruction tr tp) bl in
    let taille_pop=List.fold_left (fun a b ->  a+ (snd b) ) 0 nbl in
    let pop=string_of_int taille_pop in
    let codeb=List.fold_left (fun a b -> a^(fst b)) ("") nbl in
    codeb^"POP (0) "^pop^"\n"

  

  and analyser (Programme(lf,bl))=
  let debu= (getEntete() ) in
  let nbl=analyse_bloc Undefined 0 bl in 
  let lff = (List.map analyse_fonction lf) in
  let nlf=List.fold_left (fun x y -> x^y) "" lff in
    debu^nlf^"main \n"^nbl^"HALT \n"

  and  analyse_fonction f = 
     let (Fonction(ia,_,nb)) = f in 
     let InfoFun(nom,t_ret,l_type) = info_ast_to_info ia in
     let listtaille = List.map getTaille l_type in 
     let taille = List.fold_left (fun x y -> x+y) 0 listtaille in 
        nom^"\n"^(analyse_bloc t_ret (taille) nb)^"HALT"^"\n \n"
   
end
