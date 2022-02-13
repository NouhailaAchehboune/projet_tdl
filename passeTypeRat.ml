module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Type
  open Exceptions
  open Ast
  open AstType
  open Tds

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme
   
(* get_type_retour : Tds.info_ast -> typ *)
(* Paramètre ia : l'info ast d'une fonction *)
(* Renvoie le type de retour de l'info ast d'une fonction *)
  let get_type_retour ia =
      let InfoFun(_, tr,_) = info_ast_to_info ia in tr

(* get_type : Tds.info_ast -> typ *)
(* Paramètre ia : l'info ast d'une variable ou d'une constante *)
(* Renvoie le type d'une variable ou int pour une constante *)
  let get_type ia =
    match info_ast_to_info ia with
    | InfoVar(_,t,_,_) ->  t
    | InfoConst _ -> Int
    | _ -> failwith "" (*Fonction utilisée dans les fonctions d'analyse *)

(* get_type_param : Tds.info_ast -> typ *)
(* Paramètre ia : l'info ast d'une fonction *)
(* Renvoie la liste des types des parametres d'une fonction*)
  let get_type_param ia =
      let InfoFun(_, _, ltp) = info_ast_to_info ia in ltp

(* analyse_type_unaire : AstSyntax.unaire -> AstType.unaire *)
(* Paramètre u : l'unaire *)
(* Renvoie l'unaire correspondant du module AstType*) 
  let analyse_type_unaire u = 
    if (u=AstSyntax.Numerateur) then AstType.Numerateur
    else AstType.Denominateur

(* analyse_type_binaire : AstSyntax.binaire -> typ -> typ -> AstType.binaire *)
(* Paramètre b : le binaire *)
(* Paramètre t1 : le premier type *)
(* Paramètre t2 : le deuxième type *)
(* Vérifie la bonne utilisation des types dans les binaires et transforme le binaire en AstType.binaire selon les types trouvés*) 
(* Erreur si mauvaise utilisation des binaires avec des types non conformes *)
  let rec analyse_type_binaire b t1 t2 = 
  match b with 
    |AstSyntax.Fraction -> if (est_compatible Int t1) then Fraction (*on s'assure que le type est compatible avec le type attendu dans le binaire  *)
                  else (raise (TypeBinaireInattendu (AstSyntax.Fraction, t1, t2))) (* On lève une exception sinon *)
    |AstSyntax.Plus -> if (est_compatible Int t1) then PlusInt (*on s'assure que le type est compatible avec le type attendu dans le binaire  *)
              else if (est_compatible Rat t1) then PlusRat (*on s'assure que le type est compatible avec le type attendu dans le binaire  *)
              else (raise (TypeBinaireInattendu (AstSyntax.Plus, t1, t2))) (* On lève une exception sinon *)
    |AstSyntax.Mult -> if (est_compatible Int t1) then MultInt (*on s'assure que le type est compatible avec le type attendu dans le binaire  *)
              else if (est_compatible Rat t1) then MultRat (*on s'assure que le type est compatible avec le type attendu dans le binaire  *)
              else (raise (TypeBinaireInattendu (AstSyntax.Mult, t1, t2))) (* On lève une exception sinon *)
    |AstSyntax.Equ -> if (est_compatible Int t1) then EquInt (*on s'assure que le type est compatible avec le type attendu dans le binaire  *)
              else if (est_compatible Bool t1) then EquBool (*on s'assure que le type est compatible avec le type attendu dans le binaire  *)
              else (raise (TypeBinaireInattendu (AstSyntax.Equ, t1, t2))) (* On lève une exception sinon *)
    |AstSyntax.Inf -> if (est_compatible Int t1) then Inf (*on s'assure que le type est compatible avec le type attendu dans le binaire  *)
             else (raise (TypeBinaireInattendu (AstSyntax.Inf, t1, t2))) (* On lève une exception sinon *)

(* recherche : (typ * string)list -> string -> typ *)
(* Paramètre enreg : la liste de l'enregistrement *)
(* Paramètre nomc : le nom du champ recherché *)
(* Renvoie le type du nomc dans l'enregistrement *) 
(* Erreur si le champ recherché n'est pas trouvé dans l'enregistrement *)
  let rec recherche enreg nomc = 
    match enreg with 
    |(t,nom)::q -> if (nom=nomc) then t
                  else recherche q nomc
    |_ -> raise (TypeEnregistrementIncompatible nomc)          

(* analyse_type_affectable : AstTds.affectable -> string -> typ *)
(* Paramètre enreg : la liste de l'enregistrement *)
(* Paramètre nomc : le nom du champ recherché *)
(* Renvoie le type du nomc dans l'enregistrement *) 
(* Erreur si le champ recherché n'est pas trouvé dans l'enregistrement *)
  let rec analyse_type_affectable a=
    match a with
    |AstTds.Ident(ia) -> (AstType.Ident(ia), get_type ia)
    |AstTds.Deref(a)-> let (na,ta) = analyse_type_affectable a in (*On analyse l'affectable *)
                        begin
                          match ta with (*On verifie que le type est un pointeur *)
                            |Pointeur(t)-> (AstType.Deref(na),t)
                            |_ -> (raise (PasUnPointeur (ta))) (*On lève une exception sinon *)
                        end
    |AstTds.Acces (a1,ia) -> let (a, ta) = analyse_type_affectable a1 in (*On verifie que le type est un pointeur *)
                             let InfoVar(nom,_,_,_) = info_ast_to_info ia in (*L'info est une info_var parce que c'est vérifié dans la passe précédante*)
                          begin 
                            match ta with (*On verifie que le type est un pointeur *)
                            | Enre(le) ->  let te = (recherche le nom) in 
                                           (AstType.Acces(a,ia),te)
                            | _ -> raise (PasUnEnregistrement ta) (*On lève une exception sinon *)
                          end   

(* analyse_type_expression : AstTds.expression -> AstType.expression * typ *)
(* Paramètre e : l'expression à traiter *)
(* Vérifie la compatibilité des types utilisés et transforme l'expression en AstType.expression *)
(* Erreur si les types sont incompatibles  *) 
  let rec analyse_type_expression e = 
  match e with 
  |AstTds.AppelFonction(ia, le) -> 
    (*On analyse les expressions mises en parametres de la fonction *)
    let nlet = (List.map analyse_type_expression le) in
    (*On extrait le type de retour de la fonction *)
    let tr = (get_type_retour ia) in 
    (*On extrait la liste des types de retour de la fonction *)
    let tparam = (get_type_param ia) in
    (*On extrait la liste des expressions mises en parametres de la fonction *)
    let nle = List.map fst nlet in
    (*On extrait la liste des types des parametres de la fonction *)
    let nlp = List.map snd nlet in
    (*On teste la compatibilité des types des parametres attendus et ceux fournis *)
    if (est_compatible_list tparam nlp) then
    (AppelFonction(ia, nle), tr) else (*On appelle la fonction si c'est compatible *)
    (raise (TypesParametresInattendus (tparam, nlp))) (*On lève une exception sinon *)
  |AstTds.Affectable(a) ->let (na,type_na)=analyse_type_affectable a in (*On analyse l'affectable *)
      (AstType.Affectable(na),type_na)
  |AstTds.Booleen(b) ->
    (AstType.Booleen(b), Bool)
  |AstTds.Entier(n) ->
    (AstType.Entier(n), Int)
  |AstTds.Unaire(u, e) ->
    let ne = (analyse_type_expression e) in (*On analyse l'expression de l'unaire *)
    if (est_compatible Rat (snd ne)) then (*On teste la compatibilité du type fourni avec Rat *)
      let nu=(analyse_type_unaire u) in 
      (AstType.Unaire(nu,(fst ne)),Int)
     else (raise (TypeInattendu ((snd ne), Rat))) (*On lève une exception sinon  *)
  |AstTds.Binaire(b, e1, e2) ->
    let ne1 = (analyse_type_expression e1) in (*On analyse la première expression du binaire *)
    let ne2 = (analyse_type_expression e2) in (*On analyse la première expression du binaire *)
    if (est_compatible (snd ne1) (snd ne2)) then (*On teste la compatibilité entre les deux types  *)
      let nb = (analyse_type_binaire b (snd ne1) (snd ne2)) in
      begin  
      match nb with (*On renvoie le binaire correspondant avec le type *)
        |Fraction ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Rat )
        |PlusInt ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Int )
        |PlusRat ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Rat )
        |MultInt ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Int )
        |MultRat ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Rat )
        |EquInt ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Bool )
        |EquBool ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Bool )
        |Inf->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Bool )
      end
    else (raise (TypeBinaireInattendu (b, (snd ne1), (snd ne2)))) (*On lève une exception si les types sont incompatibles  *)
    | AstTds.Null-> ( AstType.Null , Pointeur(Undefined)) (*On renvoie le pointeur null avec le type correspondant *)
    | AstTds.New(t) -> (AstType.New(t),Pointeur(t)) (*On renvoie le pointeur avec le type correspondant *)
    | AstTds.Adresse(ia) -> begin
                            match (info_ast_to_info ia) with (*On a pas besoin de tester sur les autres infos parce que c'est déjà fait dans la passe précédente  *)
                            |InfoVar(_,t,_,_) -> (AstType.Adresse(ia),Pointeur(t))
    (*  Le cas suivant ne devrait jamais avoir lieu car verifié à la passe précédente *)
                            |_ -> failwith ""
                            end
    | AstTds.Creation(le) -> let nlt = (List.map analyse_type_expression le) in  (*On analyse l'expression de l'enregistrement crée *)
                             let nle = (List.map fst nlt) in (*On récupère les expressions  *)
                             let nlte = (List.map (fun (_,t) -> (t,"") ) nlt) in (*On ne peut pas récupérer les champs de nom du type enregistrement donc on crée un enregistrement avec des champs "" *)
                              (AstType.Creation(nle),Enre(nlte))

(* maj_typ : typ -> info_ast -> info_ast *)
(* Paramètre t : le nouveau type *)
(* Paramètre ia : l'info_ast à modifier *)
(* met à jour le type dans l'info_ast*)
let maj_type t ia = (modifier_type_info t ia)

(* analyse_type_instruction : typ option -> AstTds.instruction -> AstType.expression *)
(* Paramètre tf : le type *)
(* Paramètre i : l'instruction*)
(* Vérifie la compatibilité des types utilisés et transforme l'instruction en AstType.instruction *)
(* Erreur si les types sont incompatibles  *)
let rec analyse_type_instruction tf i = 
  match i with 
  |(AstTds.Declaration(t, ia, e)) -> (maj_type t ia); (*met à jour le type dans ia *)
    let (ne,te) = (analyse_type_expression e) in 
    if (est_compatible te t) then (*On verifie que les types sont compatibles et on làve une exception sinon *)
      begin
      AstType.Declaration(ia, ne) 
      end
    else (raise (TypeInattendu (te,t)))
  |AstTds.Affectation(a, e) -> let (na,ta) = (analyse_type_affectable a) in (*On analyse l'affectable *)
                         let (ne,te) = (analyse_type_expression e) in (*On analyse l'expression *)
                         if (est_compatible te ta) then (*On s'assure que l'affectation est possible avec la compatibilité des types *)
                          AstType.Affectation(na, ne)
                         else (raise (TypeInattendu (te,ta))) (*On lève une exception sinon *)

  |AstTds.Ajout(ia, e) -> let t = (get_type ia) in (*Onextrait le type de l'ia *)
                         let (ne,te) = (analyse_type_expression e) in (*On analyse l'expression *)
                         if (est_compatible te t) then (*On s'assure que les types sont compatibles *)
                           if (analyse_type_binaire Plus te t = PlusInt) then (*On renvoie AjoutInt si on ajoute un entier à un entier *)
                            AstType.AjoutInt(ia, ne)
                           else 
                            AstType.AjoutRat(ia, ne) (*On renvoie AjoutRat si on ajoute un Rat à un Rat *)
                         else (raise (TypeInattendu (te,t))) (*On lève une exception sinon *)
  |AstTds.Affichage(e) -> let (ne, te) = (analyse_type_expression e) in (*On extrait le type de l'expression *)
                   begin
                   match te with (*On décide si on va afficher un entier ,un rat ou un bool *)                 
                   |Int -> AstType.AffichageInt(ne)
                   |Rat ->  AstType.AffichageRat(ne)
                   |Bool ->  AstType.AffichageBool(ne)
                   |_ -> (raise (TypeInattendu (te,te))) (*On lève une exception si aucun des types cités n'est trouvé *)
                   end
  |AstTds.Conditionnelle(e,bt,be) -> let (ne, te) = (analyse_type_expression e) in (*On analyse l'expression de la condition *)
                              if not(te = Bool) then (*Si le type de l'expression n'est pas bool on lève une exception *)
                                (raise (TypeInattendu (te,Bool)))
                              else let nbt = (analyse_type_bloc tf bt) in (*On analyse les deux blocs de la conditionnelle*)
                                   let nbe = (analyse_type_bloc tf be) in
                                   AstType.Conditionnelle(ne,nbt,nbe) 
  |AstTds.Retour(e) -> 
    let (ne,te) = (analyse_type_expression e) in (*On analyse l'expression et on récupère son type *)
    begin
    match tf with  
    |None -> (raise RetourDansMain)
    |Some t -> if (t=te) then (*si le type attendu est le type de l'expression on retourne *)
                AstType.Retour(ne)
               else (raise (TypeInattendu (te,t))) (*On lève une exception sinon *)
               end
  |AstTds.TantQue(e,bl) -> let(ne, te) =
                              (analyse_type_expression e) in (*On analyse l'expression et on recupère le type *)
                              if not(te = Bool) then (*Si le type de l'expression n'est pas bool on lève une exception *)
                                (raise (TypeInattendu (te,Bool)))
                              else let nbl = (analyse_type_bloc tf bl) in (*on analyse le bloc de tant que sinon *)
                                   AstType.TantQue(ne,nbl) 
  |AstTds.Empty -> AstType.Empty 

(* analyse_type_bloc : typ option -> bloc -> bloc *)
(* Paramètre tf : le type *)
(* Paramètre li : la liste des instructions *)
(* Vérifie la compatibilité des types utilisés et transforme le bloc en AstType.bloc *)
(* Erreur si les types sont incompatibles  *)
and analyse_type_bloc tf li = List.map (analyse_type_instruction tf) li 

(* analyse_type_fonction : AstTds.Fonction -> AstType.Fonction *)
(* Paramètre Fonction : la fonction à analyser *)
(* Vérifie la compatibilité des types utilisés et transforme la Fonction en AstType.Fonction *)
(* Erreur si les types sont incompatibles  *)
let analyse_type_fonction (AstTds.Fonction(t,ia,la,b)) = 
  List.iter (fun(tp,iap) -> (modifier_type_info tp iap)) la;
  let nlpt=List.map fst la in
  modifier_type_fonction_info t nlpt ia;
  let nlpi=List.map snd la in
  let nb = analyse_type_bloc (Some t) b in 
    AstType.Fonction(ia,nlpi,nb)

(* analyser : AstTds.Programme -> AstType.Programme *)
(* Paramètre Programme : le programme à analyser *)
(* Vérifie la compatibilité des types utilisés et transforme le Programme en AstType.Programme *)
(* Erreur si les types sont incompatibles  *)
let analyser (AstTds.Programme(lf, b)) =
  let nlf = List.map (analyse_type_fonction) lf in 
  let nb = (analyse_type_bloc None b) in
    AstType.Programme(nlf, nb)
end