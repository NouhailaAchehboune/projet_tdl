module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Type
  open Exceptions
  open Ast
  open AstType
  open Tds

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme

  let get_type_retour ia =
  let InfoFun(_, tr,_) = info_ast_to_info ia in tr

  let get_type ia =
    match info_ast_to_info ia with
    | InfoVar(_,t,_,_) ->  t
    | InfoConst _ -> Int
    | _ -> failwith ""

  let get_type_param ia =
  let InfoFun(_, _, ltp) = info_ast_to_info ia in ltp
 
  let analyse_type_unaire u = 
    if (u=AstSyntax.Numerateur) then Numerateur
    else Denominateur

  let rec analyse_type_binaire b t1 t2 = 
  match b with 
    |AstSyntax.Fraction -> if (est_compatible Int t1) then Fraction
                  else (raise (TypeBinaireInattendu (AstSyntax.Fraction, t1, t2)))
    |AstSyntax.Plus -> if (est_compatible Int t1) then PlusInt
              else if (est_compatible Rat t1) then PlusRat
              else (raise (TypeBinaireInattendu (AstSyntax.Plus, t1, t2)))
    |AstSyntax.Mult -> if (est_compatible Int t1) then MultInt
              else if (est_compatible Rat t1) then MultRat
              else (raise (TypeBinaireInattendu (AstSyntax.Mult, t1, t2)))
    |AstSyntax.Equ -> if (est_compatible Int t1) then EquInt
              else if (est_compatible Bool t1) then EquBool
              else (raise (TypeBinaireInattendu (AstSyntax.Equ, t1, t2)))
    |AstSyntax.Inf -> if (est_compatible Int t1) then Inf
             else (raise (TypeBinaireInattendu (AstSyntax.Inf, t1, t2)))

  
  let rec analyse_type_affectable a=
    match a with
    |AstTds.Ident(ia) ->
    (AstType.Ident(ia), get_type ia)

  
  let rec analyse_type_expression e = 
  match e with 
  |AstTds.AppelFonction(ia, le) -> 
    let nlet = (List.map analyse_type_expression le) in
    let tr = (get_type_retour ia) in 
    let tparam = (get_type_param ia) in
    let nle = List.map fst nlet in
    let nlp = List.map snd nlet in
    if (est_compatible_list tparam nlp) then
    (AppelFonction(ia, nle), tr) else
    (raise (TypesParametresInattendus (tparam, nlp)))
  |AstTds.Affectable(a) ->let (na,type_na)=analyse_type_affectable a in
      (AstType.Affectable(na),type_na)
  |AstTds.Booleen(b) ->
    (AstType.Booleen(b), Bool)
  |AstTds.Entier(n) ->
    (AstType.Entier(n), Int)
  |AstTds.Unaire(u, e) ->
    let ne = (analyse_type_expression e) in
    if (est_compatible Rat (snd ne)) then 
      let nu=(analyse_type_unaire u) in
      (AstType.Unaire(nu,(fst ne)),Int)
     else (raise (TypeInattendu ((snd ne), Rat)))
  |AstTds.Binaire(b, e1, e2) ->
    let ne1 = (analyse_type_expression e1) in
    let ne2 = (analyse_type_expression e2) in
    if (est_compatible (snd ne1) (snd ne2)) then
      let nb = (analyse_type_binaire b (snd ne1) (snd ne2)) in
      begin  
      match nb with
        |Fraction ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Rat )
        |PlusInt ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Int )
        |PlusRat ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Rat )
        |MultInt ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Int )
        |MultRat ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Rat )
        |EquInt ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Bool )
        |EquBool ->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Bool )
        |Inf->(AstType.Binaire(nb, (fst ne1),(fst ne2)),Bool )
      end
    else (raise (TypeBinaireInattendu (b, (snd ne1), (snd ne2))))

let maj_type t ia = (modifier_type_info t ia)

let rec analyse_type_instruction tf i = 
  match i with 
  |(AstTds.Declaration(t, ia, e)) -> (maj_type t ia); 
    let (ne,te) = (analyse_type_expression e) in
    if (est_compatible te t) then
      begin
      AstType.Declaration(ia, ne) 
      end
    else (raise (TypeInattendu (te,t)))
  |AstTds.Affectation(a, e) -> let (na,ta) = (analyse_type_affectable a) in 
                         let (ne,te) = (analyse_type_expression e) in
<<<<<<< HEAD
                         if (est_compatible te ta) then 
                          AstType.Affectation(na, ne)
                         else (raise (TypeInattendu (te,ta)))
=======
                         if (est_compatible te t) then 
                          AstType.Affectation(ia, ne)
                         else (raise (TypeInattendu (te,t)))
  |AstTds.Ajout(ia, e) -> let t = (get_type ia) in 
                         let (ne,te) = (analyse_type_expression e) in
                         if (est_compatible te t) then 
                           if (analyse_type_binaire Plus te t = PlusInt) then
                            AstType.AjoutInt(ia, ne)
                           else 
                            AstType.AjoutRat(ia, ne)
                         else (raise (TypeInattendu (te,t)))
>>>>>>> 0adde7004007642b40f31cf7d61b78170ac1b82a
  |AstTds.Affichage(e) -> let (ne, te) = (analyse_type_expression e) in
                   begin
                   match te with                  
                   |Int -> AstType.AffichageInt(ne)
                   |Rat ->  AstType.AffichageRat(ne)
                   |Bool ->  AstType.AffichageBool(ne)
                   |_ -> (raise (TypeInattendu (te,te)))
                   end
  |AstTds.Conditionnelle(e,bt,be) -> let (ne, te) = (analyse_type_expression e) in 
                              if not(te = Bool) then 
                                (raise (TypeInattendu (te,Bool)))
                              else let nbt = (analyse_type_bloc tf bt) in
                                   let nbe = (analyse_type_bloc tf be) in
                                   AstType.Conditionnelle(ne,nbt,nbe) 
  |AstTds.Retour(e) -> 
    let (ne,te) = (analyse_type_expression e) in
    begin
    match tf with 
    |None -> (raise RetourDansMain)
    |Some t -> if (t=te) then
                AstType.Retour(ne)
               else (raise (TypeInattendu (te,t))) 
               end
  |AstTds.TantQue(e,bl) -> let(ne, te) =
                              (analyse_type_expression e) in 
                              if not(te = Bool) then 
                                (raise (TypeInattendu (te,Bool)))
                              else let nbl = (analyse_type_bloc tf bl) in
                                   AstType.TantQue(ne,nbl) 
  |AstTds.Empty -> AstType.Empty 

and analyse_type_bloc tf li = List.map (analyse_type_instruction tf) li 

let analyse_type_fonction (AstTds.Fonction(t,ia,la,b)) = 
  List.iter (fun(tp,iap) -> (modifier_type_info tp iap)) la;
  let nlpt=List.map fst la in
  modifier_type_fonction_info t nlpt ia;
  let nlpi=List.map snd la in
  let nb = analyse_type_bloc (Some t) b in 
    AstType.Fonction(ia,nlpi,nb)

let analyser (AstTds.Programme(lf, b)) =
  let nlf = List.map (analyse_type_fonction) lf in 
  let nb = (analyse_type_bloc None b) in
    AstType.Programme(nlf, nb)
end