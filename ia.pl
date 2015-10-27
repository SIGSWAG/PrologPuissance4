%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
:- module(ia, [iaAleatoire/1, iaMinimax/2]).

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

iaMinimax(JoueurCourant,Coup) :-
	parcoursArbre(JoueurCourant,3,Coup,_).