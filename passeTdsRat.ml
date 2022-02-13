(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme




(* analyse_tds_parametre : AstTds ->typ * string -> String *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre typ : le type du parametre *)
(* Paramètre nom : le nom du parametre *)
(* Vérifie si le parametre n'est pas déclaré deux fois dans la fonction et l'ajoute à la tds locale de la fonction  *)
(* Erreur si double déclaration du parametre *)
let analyse_tds_parametre tds (typ,nom) =
 let iv =  InfoVar (nom,Undefined, 0, "") in (*On crée une info variable et on met comme type Undefined parce que la gestion de typage se fera dans la passe suivante*)
 let ia = info_to_info_ast iv in(*On construit une info_ast *)
      match (chercherLocalement tds nom) with (* On cherche localement ( dans la tds de la fonction) l'identifiant *)
            |Some _ -> raise (DoubleDeclaration nom) (* Si l'identifiant existe donc on a une double déclaration *)
            |None -> ajouter tds nom ia; (* Sinon on ajoute le parametre dans la tds locale *)
                     (typ,ia)





(* analyse_tds_affectable : AstSyntax.affectable -> AstTds.affectable *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre a : l'affectable à analyser *)
(* Paramètre modif : True si le paramètre est à gauche et False s'il est à droite *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'affectable
en un affectable de type AstTds.affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_affectable tds modif a=
  match a with
  |AstSyntax.Ident(id) -> 
    begin
      match chercherGlobalement tds id with     
        | None -> (* L'identifiant n'est pas trouvé dans la tds globale. *) 
              raise (IdentifiantNonDeclare id)
        | Some ia ->(* L'identifiant est trouvé dans la tds globale, 
              il a donc déjà été déclaré. L'information associée est récupérée. *)  
              let i= info_ast_to_info ia in 
                    begin
                        match i with 
                        (* L'identifiant est une fonction. *) 
                        |InfoFun(_,_,_) -> raise (MauvaiseUtilisationIdentifiant id)
                        (* L'identifiant est une variable. *) 
                        |InfoVar(_,_,_,_)-> AstTds.Ident(ia)
                        (* L'identifiant est une constante. *) 
                        |InfoConst _ -> if modif then (* Si l'identifiant de la constante se trouve à droite on ne peut pas le modifier . *) 
                                            raise(MauvaiseUtilisationIdentifiant id) 
                                        else 
                                            AstTds.Ident(ia)
                    end
    end
  |AstSyntax.Deref(a1) -> let na = analyse_tds_affectable tds modif a1 in
                             AstTds.Deref(na)
  |AstSyntax.Acces (a1,n) -> let nb = analyse_tds_affectable tds modif a1 in 
      begin
            match chercherGlobalement tds n with     
            | None -> (* L'identifiant n'est pas trouvé dans la tds globale. *) 
                  raise (IdentifiantNonDeclare n)
            | Some ia ->(* L'identifiant est trouvé dans la tds globale, 
                  il a donc déjà été déclaré. L'information associée est récupérée. *)  
                  let i= info_ast_to_info ia in 
                        begin
                          match i with 
                          |InfoVar(_,_,_,_)-> AstTds.Acces(nb,ia)
                          |_ -> raise (MauvaiseUtilisationIdentifiant n) (*L'identifiant ne peut pas etre autre qu'une variable*)
                        end
      end

(* analyse_tds_expression : AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e = 
  match e with
  | AstSyntax.AppelFonction(id, le) -> 
      begin
        match chercherGlobalement tds id with 
        | None -> raise (IdentifiantNonDeclare id) (* L'identifiant n'est pas trouvé dans la tds globale. *) 
        | Some ia -> (* L'identifiant est trouvé dans la tds globale, 
                  il a donc déjà été déclaré. L'information associée est récupérée. *)  
            begin
                    match (info_ast_to_info ia) with 
                    | InfoFun(_,_,_) ->
                          let nle = (List.map (analyse_tds_expression tds) le) in (* On analyse la liste des expressions du bloc de la fonction. *) 
                                     AstTds.AppelFonction(ia, nle)
                    | _ -> raise  (MauvaiseUtilisationIdentifiant id) (*On ne peut appeler qu'une fonction*)
            end
      end
  | AstSyntax.Affectable(a) -> let na = analyse_tds_affectable tds false a in (* On analyse l'affectable et on met un false parce qu'il est à gauche. *)
                                    AstTds.Affectable(na)
  | AstSyntax.Booleen(bool) -> AstTds.Booleen(bool)
  | AstSyntax.Entier(e) -> AstTds.Entier(e)
  | AstSyntax.Unaire(u, e) -> let ne = analyse_tds_expression tds e in (* On analyse l'expression. *)
                                AstTds.Unaire(u,ne)
  | AstSyntax.Binaire(b, e1, e2) -> let ne1 = analyse_tds_expression tds e1 in (* On analyse l'expression. *)
                                    let ne2 = analyse_tds_expression tds e2 in (* On analyse l'expression. *)
                                          AstTds.Binaire(b,ne1, ne2)
  | AstSyntax.Null -> AstTds.Null
  | AstSyntax.New(t) -> AstTds.New(t)
  | AstSyntax.Adresse(n)-> begin
                              match chercherGlobalement tds n with 
                              |None -> raise (IdentifiantNonDeclare n) (* L'identifiant n'est pas trouvé dans la tds globale. *) 
                              |Some ia ->(* L'identifiant est trouvé dans la tds globale, 
                                          il a donc déjà été déclaré. L'information associée est récupérée. *) 
                                          begin
                                            match (info_ast_to_info ia) with
                                            |InfoVar _ -> AstTds.Adresse(ia)
                                            |_ -> raise  (MauvaiseUtilisationIdentifiant n) (*On ne peut accéder qu'à l'adresse d'une variable *)
                                          end
                          end
  |AstSyntax.Creation(le) -> let nle = (List.map (analyse_tds_expression tds) le ) in (* On analyse l'expression. *)
                                  AstTds.Creation(nle)              

                                  
(* analyse_tds_instruction : AstSyntax.instruction -> tds -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale, 
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *) 
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            begin
            match t with
            |Enre(lt) ->  let _nlp = List.map (analyse_tds_parametre tds) lt in (*On analyse les parametres de la fonction*)            
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
            et l'expression remplacée par l'expression issue de l'analyse *)
            Declaration (t, ia, ne)
            |_ -> Declaration (t, ia, ne)
            end
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale, 
            il a donc déjà été déclaré dans le bloc courant *) 
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (a,e) ->let na = analyse_tds_affectable tds true a in (* Analyse de l'affectable avec true parce qu'il se retrouve à droite *)
                                  let ne = analyse_tds_expression tds e in (* On analyse l'expression. *)
                                      Affectation(na,ne)
  | AstSyntax.Ajout(n,e) ->
       begin
            match chercherGlobalement tds n with
            | None -> 
              (* L'identifiant n'est pas trouvé dans la tds globale. *) 
                 raise (IdentifiantNonDeclare n)
            | Some info -> 
              (* L'identifiant est trouvé dans la tds globale, 
              il a donc déjà été déclaré. L'information associée est récupérée. *) 
              begin
                match info_ast_to_info info with
                | InfoVar _ -> 
                  (* Vérification de la bonne utilisation des identifiants dans l'expression *)
                  (* et obtention de l'expression transformée *) 
                  let ne = analyse_tds_expression tds e in
                  (* Renvoie de le nouvel ajout où le nom a été remplacé par l'information 
                  et l'expression remplacée par l'expression issue de l'analyse *)
                  Ajout (info, ne)
                |  _ ->
                  (* Modification d'une constante ou d'une fonction *)  
                  raise (MauvaiseUtilisationIdentifiant n) 
              end
        end
  | AstSyntax.Constante (n,v) -> 
      begin
        match chercherLocalement tds n with
        | None -> 
        (* L'identifiant n'est pas trouvé dans la tds locale, 
        il n'a donc pas été déclaré dans le bloc courant *)
        (* Ajout dans la tds de la constante *)
        ajouter tds n (info_to_info_ast (InfoConst (n,v))); 
        (* Suppression du noeud de déclaration des constantes devenu inutile *)
        Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale, 
          il a donc déjà été déclaré dans le bloc courant *) 
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e -> 
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds b in
      (* Renvoie la nouvelle structure de la boucle *)
      TantQue (nc, bast)
  | AstSyntax.Retour (e) -> 
      (* Analyse de l'expression *)
      let ne = analyse_tds_expression tds e in
      Retour (ne)

      
(* analyse_tds_bloc : AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc 
  Cette tds est modifiée par effet de bord *)
  let nli = List.map (analyse_tds_instruction tdsbloc) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli



(* analyse_tds_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  =
    match chercherGlobalement maintds n with   (*On fait une recherche globale de la fonction *)
    | None ->let ia = (info_to_info_ast (InfoFun(n, Undefined,[]))) in  (* On crée unne info_ast de la fonctio, *)   
             let tdsfunction = creerTDSFille maintds in  (*On crée une Tds fille pour la fonction *)
             let nlp = List.map (analyse_tds_parametre tdsfunction) lp in (*On analyse les parametres de la fonction*)
             let nb = analyse_tds_bloc tdsfunction li in (* On analyse le bloc de la fonction dans la tds fonction *)
                 (ajouter maintds n ia); (* On ajoute l'info tds de la fonction dans la tds globale  *)
                 Fonction(t,ia,nlp,nb) 
    | Some _ -> raise (DoubleDeclaration n) (* On leve une exception s'il y a une double déclaration de la fonction *)


(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in (*On cree une TDS mère pour le programme *)
  let nf = List.map (analyse_tds_fonction tds) fonctions in (* On analyse les fonctions présentes dans le programme avec la tds courante *)
  let nb = analyse_tds_bloc tds prog in (*On analyse le bloc du programme *)
        Programme (nf,nb)
end
