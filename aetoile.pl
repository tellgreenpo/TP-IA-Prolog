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

expand([[_,_,G],U],L):-
	findall([U1, [F1, H1, G1], U, Mv],(rule(Mv,1,U,U1), heuristique(U1,H1), G1 is G+1, F1 is H1+G1),L).

aetoile([], [], _) :- write("Pas de solution").
aetoile(Pf, _, _) :-
	suppress_min(([_, U]), Pf, _),
	final_state(U),
	write_state(U).
aetoile(Pf, Pu, Q) :-
	suppress_min([[F,H,G],U],Pf,Pf_res),
	suppress([U,[F,H,G],U_Prev,Mv], Pu, Pu_res),
	 % Suppression noeud frr
	% determination tous les noeuds fils et calcul evaluation
	expand([[F,H,G],U],L),
	%loop_successors(L,Q,Pu,Pf),
	insert([U,[F,H,G],U_Prev,Mv],Q,Q_update),
	aetoile(Pf_res,Pu_res,Q_update).


%======================= ASSIGN FUNCTION ===========================

assign(Old,Old).


%======================= LOOP SUCCESSORS ===========================

% Trivial case : No element to explore
loop_successors([],_,Pu,Pf,Pu,Pf):-
	writeln("Finished"),
	writeln("Assigned").

% Trivial case : Element belongs to Q (Already explored)
loop_successors([[U,[_,_,_],_,_]|T],Q,Pu,Pf,Pu_update,Pf_update):-
	belongs(U,Q),
	writeln("Already explored"),
	loop_successors(T,Q,Pu,Pf,Pu_update,Pf_update).

% Non-Trivial case : Element is already in Pu -> Keep the best one
loop_successors([[U,[F_new,G_New,H_new],Pere_new,Mv_new]|T],Q,Pu,Pf,Pu_update,Pf_update):-
	belongs(U,Pu),
	writeln("Belongs to Pu"),
	% Keep the one with the best heuristic
	suppress([U,[F_old,G_old,H_old],_,_],Pu,Pu_res),
	(F_old =< F_new ->
		loop_successors(T,Q,Pu,Pf,Pu_update,Pf_update)
	;
		(suppress([[F_old,G_old,H_old],U],Pf,Pf_res),
		insert([U,[F_new,G_New,H_new],Pere_new,Mv_new],Pu_res,Pu_final),
		insert([[F_new,G_New,H_new],U],Pf_res,Pf_final),
		loop_successors(T,Q,Pu_final,Pf_final,Pu_update,Pf_update)
		)
	).
% Non-Trivial case : It is a new situation
loop_successors([[U,[F_new,G_New,H_new],Pere_new,Mv_new]|T],Q,Pu,Pf,Pu_update,Pf_update):-
	writeln("New situation"),
	insert([U,[F_new,G_New,H_new],Pere_new,Mv_new],Pu,Pu_final),
	insert([[F_new,G_New,H_new],U],Pf,Pf_final),
	loop_successors(T,Q,Pu_final,Pf_final,Pu_update,Pf_update).

%=====================================================================

affiche_solution([]).
affiche_solution([[U1,[F1,H1,G1],Pere1,Mv1]|T]):-
	write(U1),nl,
	write(F1),nl,
	write(H1),nl,
	write(G1),nl,
	write(Pere1),nl,
	write(Mv1),nl,
	nl,
	affiche_solution(T).


test_expand(L):-
	initial_state(Ini),
	heuristique(Ini, H0),
	G0 is 0,
	F0 is H0 + G0,
	expand([[F0,G0,H0],Ini],L),
	affiche_solution(L).

test_loop(1):-
	initial_state(Ini),

	% calcul de F0 (longueur totale du chemin G+H), H0 (heuristique) et G0 (distance parcourue entre Ini et U)
	heuristique(Ini, H0),
	G0 is 0,
	F0 is H0 + G0,

	% initialisations Pf, Pu et Q
	empty(Pf),insert([[F0,H0,G0],Ini],Pf,Pf1),
	empty(Pu),insert([Ini,[F0,H0,G0],nil,nil],Pu,Pu1),
	empty(Q),insert([Ini,[F0,H0,G0],nil,nil],Q,Q1),
	empty(Pu_update),empty(Pf_update),
	expand([[F0,H0,G0],Ini],L),
	loop_successors(L,Q,Pu,Pf,Pu_update,Pf_update),
	writeln("finished loop_succesors"),
	put_flat(Pu),nl,
	put_flat(Pf),nl,
	put_flat(Pu_update),nl,
	put_flat(Pf_update),nl.

test_loop(2):-
	initial_state(Ini),

	% calcul de F0 (longueur totale du chemin G+H), H0 (heuristique) et G0 (distance parcourue entre Ini et U)
	heuristique(Ini, H0),
	G0 is 0,
	F0 is H0 + G0,

	% initialisations Pf, Pu et Q
	empty(Pf),insert([[F0,H0,G0],Ini],Pf,Pf1),
	empty(Pu),insert([Ini,[F0,H0,G0],nil,nil],Pu,Pu1),
	empty(Q),insert([Ini,[F0,H0,G0],nil,nil],Q,Q1),
	expand([[F0,H0,G0],Ini],L),
	loop_successors(L,Q1,Pu1,Pf1,B,C),
	put_flat(Q1),
	nl,
	put_flat(Pu1),
	nl,
	put_flat(Pf1),
	nl,
	writeln("Update Pu:"),
	put_flat(B),
	nl,
	writeln("Update Pf:"),
	put_flat(C).
