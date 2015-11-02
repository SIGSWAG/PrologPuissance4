%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
:- module(ia, [iaAleatoire/1, iaMinimax3/2, iaMinimax4/2, iaMinimax5/2]).

:- use_module(jeu).
:- use_module(util).
:- use_module(miniMax).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :-
	nbColonnes(NBCOLONNES),
	Coup is random(NBCOLONNES)+1,
	coupValide(Coup).
% ia aleatoire a choisi une colonne pleine, donc on la fait recommencer
iaAleatoire(Coup) :-
	iaAleatoire(Coup).

iaMinimax3(JoueurCourant,Coup) :-
	parcoursArbre(JoueurCourant,3,Coup,_).

iaMinimax4(JoueurCourant,Coup) :-
	parcoursArbre(JoueurCourant,4,Coup,_).

iaMinimax5(JoueurCourant,Coup) :-
	parcoursArbre(JoueurCourant,5,Coup,_).