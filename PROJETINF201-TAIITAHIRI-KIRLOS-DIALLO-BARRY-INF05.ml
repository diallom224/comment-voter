(* PROJET INF201 OCaml *) 

(* TAII TAHIRI YASSMINE    KIRLOS YOUSSEF    DIALLO MAMADOU       BARRY SALIMATOU  *)  (* INF5 *) 

(* QUESTION 1 *)

type bulletin = string ;; 
type candidat = bulletin ;;
type urne = bulletin list ;;
type score = int ;;
type panel = candidat list ;; 

(* On a utilisé ces 4 variables pour les exemples de la spécification *) 
let lc1:panel = ["Eric";"Kyle";"Stan"] ;;
let u1:urne = ["Eric";"Kyle";"Stan";"Kyle";"Kyle";"Stan";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric"; "Eric";"Eric";"Stan";"Stan"] ;;
let lc2 = ["Eric";"Kyle";"Stan";"Keny"];;
let u2 = ["Keny";"Kyle";"Keny";"Kyle";"Kyle";"Keny";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";
          "Eric";"Eric";"Stan";"Stan"];;


(* QUESTION 2 *)

let rec compte (c:candidat) (u:urne):score=
  match u with 
  | [] -> 0 
  | e::f -> compte c f + (if e=c then 1 else 0) ;;


(* SPECIFICATION
Profil : compte : candidat -> urne -> score 
Sémantique : La fonction compte renvoie le nombre d’occurrence du candidat c dans l’urne u.
Exemple : 
(I) compte "Eric" ["Kyle";"Stan"] = 0
(II) compte "Eric" ["Eric";"Kyle";"Stan"] = 1
(III) compte "Eric" ["Eric";"Eric";"Eric"] = 3
*)


(* QUESTION 3 *)

type resultat = (candidat*score) list ;;

let rec depouiller (p:panel) (u:urne):resultat=
  match p with 
  | [] -> [] 
  | e::f -> (e,compte e u)::depouiller f u ;;
  
(*SPECIFICATION
Profil : depouiller : panel -> urne -> resultat 
Sémantique : La fonction depouiller renvoie la liste des candidats et leur score.
Exemple : 
(I) depouiller lc1 u1 = [("Eric", 7); ("Kyle", 4); ("Stan", 5)]
	(II) depouiller lc1 (["Kyle";"Eric"]) = [("Eric", 1); ("Kyle", 1); ("Stan", 0)]
*)
           

(* QUESTION 4 *)   

let rec union (r1:resultat) (r2:resultat):resultat= 
  match r1,r2 with 
  | [],[] -> [] 
  | a::b,c::d -> (fst(a),snd(a)+snd(c)):: union b d 
  | _ -> [] ;;
         
(* SPECIFICATION
Profil : union : resultat -> resultat -> resultat 
Sémantique : La fonction union renvoie les candidats et leur score obtenu entre deux bureaux de votes.
Exemple : 
(I) union [("Eric", 7); ("Kyle", 4)] [("Eric", 3); ("Kyle", 5)] = [("Eric", 10); ("Kyle", 9)]
	(II) union [("Kyle", 3); ("Stan", 0)] [("Kyle", 1); ("Stan", 0)] = [("Kyle", 4); ("Stan", 0)]
*)
             
  
(* QUESTION 5 *) 

let rec max_depouiller (r:resultat):(candidat*score) = 
  match r with 
  |[] -> failwith "Liste Vide" 
  |[e] -> e
  |e::f -> let max = max_depouiller f in 
      let (a,b) = max in 
      if snd(e) > b then (fst(e),snd(e)) else (a,b) ;; 

(*SPECIFICATION
Profil : max_depouiller : resultat -> candidat*score 
Sémantique : LA fonction max_depouiller renvoie le candidat qui a le meilleur score et son score. 
Exemple : 
(I) max_depouiller [("Eric", 7); ("Kyle", 4); ("Stan", 5)] = ("Eric", 7)
	(II) max_depouiller [("Kyle", 4); ("Stan", 0)] = ("Kyle", 4)
*)


(* QUESTION 6 *)

let vainqueur_scrutin_uninominal (u:urne) (p:panel):candidat=
  fst(max_depouiller(depouiller p u)) ;;

(*SPECIFICATION
Profil : vainqueur_scrutin_uninominal  : urne -> panel -> candidat 
Sémantique : La fonction vainqueur_scrutin_uninominal renvoie le vainqueur de l’élection (le candidat ayant obtenu le plus de voix). 
Exemple : 
(I) vainqueur_scrutin_uninominal u1 lc1 = "Eric"
*)


(* QUESTION 7 *)

let rec suppr_elem (l:'a list) (el:'a): 'a list= 
  match l with 
  | [] -> []
  | e::f -> if (e=el) then f else e::suppr_elem f el ;;

let deux_premiers (u:urne) (lc:panel): (candidat * score) * (candidat * score) = 
  match lc with
  | []-> failwith "Erreur"
  | _ -> (max_depouiller(depouiller lc u),max_depouiller(suppr_elem (depouiller lc u) (max_depouiller(depouiller lc u))))

(*SPECIFICATION
Profil : suppr_elem  : ‘a list -> ‘a -> ‘a list 
Sémantique : La fonction suppr_elem supprime l'occurrence de l’élément e dans une liste.
Exemple : 
(I) suppr_elem [("Eric", 7); ("Kyle", 4); ("Stan", 5)] ("Eric", 7) = [("Kyle", 4); ("Stan", 5)] 
(II) suppr_elem ["Eric";"Kyle";"Eric";"Stan"] "Eric" = ["Kyle";"Eric";"Stan"]
*)

(*SPECIFICATION
Profil : deux_premiers  : urne -> panel -> (candidat * score) * (candidat * score)
Sémantique : La fonction deux_premiers renvoie les deux candidats ayant obtenu le plus de voix avec leur score. 
Exemple : 
(I) deux_premiers u1 lc1 = (("Eric", 7), ("Stan", 5))
*)


(* QUESTION 8 *)

(* Quel est le probleme : dans ce cas precis, le probleme reside dans le fait que 
  l'ajout d'un quatrieme candidats peut changer les resultats de l'election.
   en effet si certains electeurs reportent leurs votes sur ce candidat, cela peut
   modifier le resultat du scrutin et faire en sorte que le vainqueur ne soit plus le meme 
   qu'en l'abscence du quatrieme candidat. ce phenomene illustre le fait que le mode de scrutin 
   uninominal peut etre sensible au changement de preferences individuelles et peut ne pas 
   refleter les preferences collectives de maniere satisfaisante. *)



(* QUESTION 9 *)

(*
  la source du probleme dans le scrutin uninominal reside dans le fait qu'il ne permet 
    pas de refleter la diversite des opinions et des preferences des electeurs. En effet, il se contente de 
    designer un vainqueur qui peut ne pas etre representatif des choix de l'ensemble des 
    votants. De plus,l'obligation de choisir entre deux tours limite le choix des electeurs et peut conduire 
    a des situations ou le vainqueur n'est pas celui qui aurait ete choisi si tous les candidats
avait ete presents des le premier tour. Le paradoxe D'Arrow met en lumiere cette dificulte 
  en montrant que l'agregation des preferences individuelles peut conduire  a des resultats incoherants
  et contradictoires. *) 

(* QUESTION 10 *) 

(*
  En conclusion, les trois méthodes de vote présentées dans ce devoir ont chacune leurs avantages et leurs inconvénients.
  Le scrutin uninominal à deux tours est simple à comprendre et à mettre en place, mais il peut mener à des situations 
  où un candidat qui ne représente pas la majorité des électeurs est élu.
  Le jugement majoritaire permet de mieux refléter la volonté des électeurs en prenant en compte la nuance des opinions, mais 
  il est plus complexe à mettre en place et peut nécessiter un temps de traitement plus important.
  le recomptage des voix permet de garantir l’exactitude du décompte des voix, mais il ne prend pas en compte la question
  de la représentativité.

  Un avantage du scrutin uninominal à deux tours est qu’il permet de limiter le nombre de candidats en lice, ce qui 
  facilite le choix pour les électeurs. De plus, il est utilisé depuis longtemps en France et les électeurs sont habitués à ce système, ce qui
  contribue à sa légitimité. 


  Si on avait eu recours à d’autres modes de scrutin en France, le paysage politique aurait certainement été différent.
  Par exemple, si on avait utilisé le jugement majoritaire lors de l’élection présidentielle de 2017, cela aurait pu donner
  un résultat différent, car Emmanuel Macron a été élu avec un pourcentage relativement faible de voix au premier tour. 
  De même, si on avait utilisé le scrutin de Condorcet lors des élections législatives de 2017, cela aurait pu modifier la composition
  de l’Assemblée nationale, car ce mode de scrutin prend mieux en compte les nuances des opinions politiques. Cependant, il est
  difficile de prédire avec certitude les effets de tels changements sur le paysage politique français. *) 


(*Q11*)
type mention = Tresbien | Bien | Assezbien | Passable | Insuffisant | Arejeter
type bulletin_jm = mention list
type urne_jm = bulletin_jm list 
    
let urne_test=[[Tresbien; Assezbien; Arejeter; Passable]; 
               [Assezbien; Assezbien; Arejeter; Tresbien]; 
               [Tresbien; Arejeter; Arejeter; Tresbien]];;
    
  
(*Q12*) 
let rec depouiller_jm ( u:urne_jm) : urne_jm =
  match u with 
  |[]->[]
  |h::t->if h=[] then [] else (List.map List.hd u) ::(depouiller_jm (List.map List.tl u ));;
(*SPECIFICATION
Profil : depouiller_jm : urne_jm -> urne_jm
Sémantique : Une fonction qui associe a chaque candidats l’ensemble des mentions qu’il obtient
a partir des bulletins presents dans l’urne.
Exemples:   
(I) depouiller_jm urne_test = [[Tresbien; Assezbien; Tresbien]; 
                              [Assezbien; Assezbien; Arejeter];
                              [Arejeter; Arejeter; Arejeter]; 
                              [Passable; Tresbien; Tresbien]]
*)




 (*Q13*)
let mentionVnum (m:mention):int=
  match m with 
  |Arejeter->0
  |Insuffisant->1
  |Passable->2
  |Assezbien->3
  |Bien->4
  |Tresbien->5 ;; 
(*SPECIFICATION
Profil : mentionVnum : mention -> int
Sémantique : La fonction associe a chaque mention un entier pour pouvoir les comparer par la suite;
Exemples: 
    (I) mentionVnum Arejeter = 0 
*)

let tri (l:bulletin_jm):bulletin_jm=
  let insertion (l:bulletin_jm) (x:mention):bulletin_jm=
    List.fold_right (fun mtn acc -> if mentionVnum(mtn)<=mentionVnum(List.hd(acc)) 
                      then mtn::acc 
                      else List.hd(acc)::mtn::List.tl(acc)) l [x] 
  in List.fold_left insertion  [] l;;
(*SPECIFICATION
Profil : tri : bulletin_jm -> bulletin_jm
Sémantique : La fonction tri les mentions de la liste en ordre croissant
Exemples: 
    (I) tri [Tresbien ; Bien ; Passable] = [Passable; Bien; Tresbien]
*)

let tri_mentions (lm : urne_jm):urne_jm= 
  List.map tri lm;;
(*SPECIFICATION
Profil : tri_mentions : urne_jm -> urne_jm
Sémantique : La fonction trie les mentions de chaques candidats par ordre croissant.
Exemples: 
    (I) tri_mentions (depouiller_jm urne_test) = [[Assezbien; Tresbien; Tresbien]; 
                                                 [Arejeter; Assezbien; Assezbien];
                                                 [Arejeter; Arejeter; Arejeter]; 
                                                 [Passable; Tresbien; Tresbien]]
*)


(*Q14*)
let mediane (l_triee:bulletin_jm):mention= 
  let longueur = List.length l_triee in
  if longueur=0 then failwith "Erreur"
  else List.nth l_triee (longueur / 2) ;;
(*SPECIFICATION
Profil : mediane : bulletin_jm -> mention
Sémantique : La fonction renvoie la mediane d’une liste triée qui par convention est l'element d'indice n/2.
Exemples: 
    (I) mediane [Tresbien ; Bien ; Passable] = Bien
*) 

(*Q15*)
let meilleur_mediane (u_liste:urne_jm):mention=
  let u_l_triee=tri_mentions u_liste in 
  List.hd (List.rev(tri (List.map mediane u_l_triee)));; 
(*SPECIFICATION
Profil : meilleur_mediane : urne_jm -> mention
Sémantique : La fonction renvoie la meilleure mediane a partir d’une liste contenant les mentions de chaque candidats
Exemples: 
    (I) meilleur_mediane (depouiller_jm urne_test) = Tresbien 
*)

(*Q16*) 
let supprime_perdants (u_liste:urne_jm):urne_jm=
  let u_l_triee=tri_mentions u_liste in 
  let m_mediane=meilleur_mediane u_l_triee in 
  List.map (fun l_elem -> 
      if mentionVnum (mediane l_elem) < mentionVnum m_mediane 
      then [] else l_elem) u_l_triee ;;
(*SPECIFICATION
Profil : supprime_perdants : urne_jm -> urne_jm
Sémantique : La fonction supprime tous les candidats qui ont une mediane inferieur
a la meilleure mediane et remplace leur liste par une liste vide.
Exemples:
    (I) supprime_perdants (depouiller_jm urne_test) = [[Assezbien; Tresbien; Tresbien]; []; []; [Passable; Tresbien; Tresbien]]
*)




(*Q17*)
let rec supprime_mention (m_a_supp:mention) (l:bulletin_jm):bulletin_jm=
  match l with 
  |[]->[]
  |h::t->if h = m_a_supp then t 
      else h::supprime_mention m_a_supp t ;;
(*SPECIFICATION
Profil : supprime_mention : mention-> bulletin_jm -> bulletin_jm
Sémantique : La fonction supprime la première occurrence d'une mention dans une liste
Exemples: 
    (I) supprime_mention Bien [Bien;Passable;Bien] = [Passable;Bien]
*)

let supprime_meilleure_mediane (u_liste:urne_jm):urne_jm=
  let u_l_nonvide=List.filter (fun l_elem -> List.length l_elem!=0) u_liste in  
  let m_mediane=meilleur_mediane u_l_nonvide in 
  List.map (fun l_elem -> if l_elem=[] then [] 
             else supprime_mention m_mediane l_elem) u_liste;; 
(*SPECIFICATION
Profil : supprime_meilleure_mediane : urne_jm -> urne_jm
Sémantique : La fonction supprime la meilleure mediane de chaque candidat
Exemples: 
    (I) supprime_meilleure_mediane [[Assezbien; Tresbien; Tresbien]; []; []; [Passable; Tresbien; Tresbien]] 
      = [[Assezbien; Tresbien]; []; []; [Passable; Tresbien]]
*)


(*Q18*)
let rec vainqueur_jm (u_candidat:urne_jm): string=
  let u_c_restant=List.filter (fun l_elem -> List.length l_elem!=0) u_candidat in
  match u_c_restant with 
  |[] -> ""
  |[e]->"Il y a un vainqueur"
  |_->let u_c_triee = tri_mentions u_c_restant in
      let u_c_sans_perdants = supprime_perdants u_c_triee in 
      let u_c_sans_meilleure_mediane = supprime_meilleure_mediane u_c_sans_perdants in
      vainqueur_jm u_c_sans_meilleure_mediane;;
(*SPECIFICATION
Profil : vainqueur_jm : urne_jm -> string 
Sémantique : La fonction renvoie le candidat vainqueur si il y a,sinon, elle renvoie une chaine vide
  (I) vainqueur_jm [[Assezbien; Tresbien]; []; []; [Passable; Tresbien]] = ""
*)


(*Q19*)
(*
  let rec trouve_vainqueur_jm (u:urne_jm) : string = 
    let ms = depouiller_jm u in 
    vainqueur_jm (tri_mentions ms) ;; 
  *) 
                                 (* 4- Recomptons les voix *)
  
type ville = string ;;
type region = string ;;
type vote = string*int
type arbre = Vide | N of arbre*arbre list | Bv of ville*vote list | Reg of region | Dpt of region ;;


let ara =
  N (Reg "Auvergne-Rhône-Alpes",
     [N (Dpt "Drôme",
         [Bv ("Valence",
              [("ARTHAUD", 161); ("ROUSSEL", 595); ("MACRON", 7756); ("LASSALLE", 590);
               ("LE PEN", 4679); ("ZEMMOUR", 2080); ("MÉLENCHON", 8398);
               ("HIDALGO", 519); ("JADOT", 1701); ("PÉCRESSE", 1423); ("POUTOU", 186);
               ("DUPONT-AIGNAN", 573)]);
          Bv ("Romans-sur-Isère",
              [("ARTHAUD", 181); ("ROUSSEL", 371); ("MACRON", 4030); ("LASSALLE", 334);
               ("LE PEN", 3270); ("ZEMMOUR", 1072); ("MÉLENCHON", 4108);
               ("HIDALGO", 251); ("JADOT", 850); ("PÉCRESSE", 631); ("POUTOU", 111);
               ("DUPONT-AIGNAN", 341)])]);
      N (Dpt "Isère",
         [Bv ("Meylan",
              [("ARTHAUD", 28); ("ROUSSEL", 169); ("MACRON", 4457); ("LASSALLE", 164);
               ("LE PEN", 1288); ("ZEMMOUR", 928); ("MÉLENCHON", 2198);
               ("HIDALGO", 251); ("JADOT", 906); ("PÉCRESSE", 763); ("POUTOU", 64);
               ("DUPONT-AIGNAN", 162)]);
          Bv ("Echirolles",
              [("ARTHAUD", 104); ("ROUSSEL", 506); ("MACRON", 3276); ("LASSALLE", 259);
               ("LE PEN", 2737); ("ZEMMOUR", 779); ("MÉLENCHON", 5121);
               ("HIDALGO", 223); ("JADOT", 590); ("PÉCRESSE", 360); ("POUTOU", 92);
               ("DUPONT-AIGNAN", 202)]);
          Bv ("Fontaine",
              [("ARTHAUD", 55); ("ROUSSEL", 363); ("MACRON", 2111); ("LASSALLE", 146);
               ("LE PEN", 1835); ("ZEMMOUR", 541); ("MÉLENCHON", 3113);
               ("HIDALGO", 185); ("JADOT", 493); ("PÉCRESSE", 212); ("POUTOU", 83);
               ("DUPONT-AIGNAN", 121)]);
          Bv ("Saint-Martin-d'Hères",
              [("ARTHAUD", 58); ("ROUSSEL", 436); ("MACRON", 2769); ("LASSALLE", 207);
               ("LE PEN", 2289); ("ZEMMOUR", 661); ("MÉLENCHON", 4763);
               ("HIDALGO", 242); ("JADOT", 777); ("PÉCRESSE", 300); ("POUTOU", 119);
               ("DUPONT-AIGNAN", 161)]);
          Bv ("Gières",
              [("ARTHAUD", 16); ("ROUSSEL", 66); ("MACRON", 1071); ("LASSALLE", 84);
               ("LE PEN", 641); ("ZEMMOUR", 205); ("MÉLENCHON", 844); ("HIDALGO", 96);
               ("JADOT", 301); ("PÉCRESSE", 155); ("POUTOU", 30);
               ("DUPONT-AIGNAN", 61)]);
          Bv ("Grenoble",
              [("ARTHAUD", 256); ("ROUSSEL", 1300); ("MACRON", 15968);
               ("LASSALLE", 845); ("LE PEN", 6444); ("ZEMMOUR", 3389);
               ("MÉLENCHON", 24568); ("HIDALGO", 1488); ("JADOT", 5644);
               ("PÉCRESSE", 2019); ("POUTOU", 508); ("DUPONT-AIGNAN", 661)])])]);;



(*Q20*)
(*Le jugement majoritaire est une méthode qui résoud le problème du "vote utile"
  en permettant aux électeurs d'exprimer leur préférence pour chaque
  candidat de manière plus nuancée.Plutôt que simplement de choisir un candidat comme
    dans le scrutin uninominal à un tour,les électeurs notent chaque candidat en fonction de 
      leur niveau d'approbation,par exemple en utilisant une échelle de 0 à 5.Les candidats
  sont ensuite classés en fonction de leur score médian, et le candidat ayant le score médian plus élévé est élu.
  Ce système de vote permet aux électeurs de faire une distinction claire entre les candidats en fonction de leurs 
  préférences individuelles,pulutôt que de simplement choisir deux options.Cela peut aider à éviter les situations
  où un candidat peut aimé mais considéré comme "moins pire"est élu simplement parcequ'il est perçu comme le seul capable 
  de battre un autre candidat.
    Une critique possible du jugement majoritaire est que cela  peut être difficile à comprendre pour les électeurs
      et à mettre en oeuvre pour les respnsables des élections.En outre,il peut être difficile d'établir une échelle
  cohérente pour évaluer les candidats,ce qui peut entrainer des différences de notation entre les électeurs.De plus le
  calcul de la médianne peut être problématique si le nombre de votants est faible ou si les évaluations sont très dispersées.*)


(*Q21*)
(*Il n' y a pas de définitions explicite des types "ville","zone","arbre",dans l'exemple donné.
  Cependant on peut déduire leur signification à partir de la structure de l'arbre fourni:
.Une "ville" est représentée par un noeud terminal contenant un nom de ville et une liste 
  d'associations de noms de candidats et de nombres de voix.
.Une "zone" est représentée par un noeud interne contenant un nom de zone 
  (par exemple un arrondissement ou une ville) et une liste de villes.
.Un "arbre" est une structure hiérarchique composée de noeuds internes (représentant des zones)
  et de noeuds terminaux (représentant des villes).
Il convient de noter que ces noms de types sont purement hypothétiques et ne sont pas utilisés 
  dans l'exemple donné. Le code fourni définit plutôt une structure de données spécifique pour stocker les résultats
  d'une élection, et les noms de variables utilisés dans le code (tels que "N", "Bv" et "Dpt") reflètent
  cette structure plutôt que des concepts généraux tels que "ville", "zone" et "arbre".*)



(*Q22*) 

let rec trouve_bv (arbre:arbre) (nom_bv:ville) =
  match arbre with
  | Vide -> failwith "Arbre Vide"
  | N (_, fils) -> List.fold_left (fun res fils -> if res = [] then trouve_bv fils nom_bv else res) [] fils
  | Bv (nom, resultat) -> if nom = nom_bv then resultat else []
  | Reg _ -> []
  | Dpt _ -> [] ;;
(*SPECIFICATION
Profil : trouve_bv: arbre -> ville -> vote list
Sémantique : une fonction qui extrait le r´esultat d’un bureau de vote `a partir d’un arbre et du nom du bureau de vote.
  (I) trouve_bv ara "Gieres" = [("ARTHAUD", 16); ("ROUSSEL", 66); ("MACRON", 1071); ("LASSALLE", 84);
                                ("LE PEN", 641); ("ZEMMOUR", 205); ("M´ELENCHON", 844); ("HIDALGO", 96);
                                ("JADOT", 301); ("P´ECRESSE", 155); ("POUTOU", 30); ("DUPONT-AIGNAN", 61)]
*)


(*Q23*)

let grenoble = trouve_bv ara "Grenoble" ;;
let fontaine = trouve_bv ara "Fontaine" ;;
let valence = trouve_bv ara "Valence" ;;
let resultat_presidentielle = union (union (grenoble) (fontaine)) (valence) ;;





(*Conclusion*)
(*La méthode du scrutin uninominal a plusieurs défauts. Tout d'abord, elle peut conduire à une sous-représentation des minorités,
  car un candidat peut remporter une élection même s'il n'a pas reçu la majorité des voix. De plus, elle peut encourager le vote stratégique, 
  où les électeurs votent pour le candidat le plus susceptible de gagner plutôt que pour leur candidat préféré.
D'un autre côté, le jugement majoritaire peut également présenter certains défauts. Par exemple, 
  il peut être difficile pour les électeurs de comprendre comment il fonctionne et comment ils doivent évaluer les candidats.
  De plus, le système peut être vulnérable à la manipulation si les électeurs notent un candidat de manière disproportionnée pour les avantages
  ou les désavantages qu'ils perçoivent pour eux-mêmes, plutôt que pour les qualités du candidat.

En fin de compte, chaque méthode de vote a ses avantages et ses inconvénients, et il est important de trouver un système qui reflète
les préférences et les valeurs de la société dans son ensemble. Des recherches supplémentaires peuvent être nécessaires pour déterminer
quel système de vote est le plus équitable et le plus efficace pour représenter les voix de tous les électeurs.*)



(* TESTS *) 

let resultat=depouiller_jm urne_test;;
tri_mentions resultat;; 
meilleur_mediane resultat;;


let resultat2=supprime_perdants resultat;;

let resultat3=supprime_meilleure_mediane resultat2;;
vainqueur_jm resultat3;;

  

