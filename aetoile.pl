%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :-
	% récupération de l'état initial de taquin.pl
	initial_state(Ini),

	% calcul de F0 (longueur totale du chemin G+H), H0 (heuristique) et G0 (distance parcourue entre Ini et U)
	heuristique(Ini, H0),
	G0 is 0,
	F0 is H0 + G0,

	% initialisations Pf, Pu et Q 
	empty(Pf),insert([[F0,H0,G0],Ini],Pf,Pf1),
	empty(Pu),insert([Ini,[F0,H0,G0],nil,nil],Pu,Pu1),
	empty(Q),insert([Ini,[F0,H0,G0],nil,nil],Q,Q1),

	% lancement de Aetoile
	aetoile(Pf1, Pu1, Q1).


%*******************************************************************************

expand([[F,H,G],U],L):-
	findall([U1, [F, H, G], U, Mv],(rule(Mv,1,U,U1), heuristique(H1), G1 is G+1, F1 is H1+G1),L).
	

aetoile([], [], _) :- write("Pas de solution").
aetoile(Pf, Pu, Q) :-
	suppress_min(([_, U]), Pf, Pf_res),
	final_state(U), 
	write_state(U).
aetoile(Pf, Pu, Q) :-
	suppress_min([U,[F,H,G],U_Pre,Mv], Pu, Pu_res),
	suppress([[F,H,G],U],Pf,Pf_res), % Suppression noeud frr
	% determination tous les noeuds fils et calcul evaluation


	
	

	
   