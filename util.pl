%%%%%%%%%%%% util.pl %%%%%%%%%%%%

:- module(util, [incr/2, decr/2]).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% incr/2(+X, -X1)
% unifie X1 à X+1
% vrai pour X1 = X+1
incr(X,X1):- 
	X1 is X+1.

% decr/2(+X, -X1)
% unifie X1 à X-1
% vrai pour X1 = X-1
decr(X,X1):- 
	X1 is X-1.

caseVideTest(X,Y) :- 
	nonvar(X),
	nonvar(Y),
	not(caseTest(X,Y,_)).

addToList(X, L, [X|L]). 

ligne(1).
ligne(2).
%parcoursArbre



% +J player qui doit jouer
% +Pmax prof maximale
% -R le coup a jouer
% +Value évaluation du noeud courant

parcoursArbre(J,Pmax,R,Value):-
	assert(maximizer(J)),
	assert(joueurCourant(J)),
	parcours(1,1,Pmax,[1,0]),
	parcours(2,1,Pmax,[2,0]),
	feuille([1,0],X1),
	feuille([2,0],X2),
	coupAJouerMaximizer([X1,X2],R,Value),
	!.

parcours(X, P, Pmax, L):-
	P==Pmax,
	nl, write('feuille'),
	print(L),
	evaluate(Value),
	assert(feuille(L, Value)).
parcours(X, P, Pmax, L) :-
	incr(P, P1),
	parcours(1, P1,Pmax, [1|L]), 
	parcours(2, P1,Pmax,[2|L]), 
	feuille([1|L], X1),feuille([2|L], X2),
	setJoueur(P1),
	choixValeurNoeud([X1,X2],R,Valeur),
	assert(feuille(L,Valeur)),
	nl,joueurCourant(Joueur),write(P1),write(Joueur), write('feuille'), print(L).

setJoueur(P):-
	parite(P),
	maximizer(jaune),
	joueurCourant(jaune),
	retract(joueurCourant(X)),
	assert(joueurCourant(rouge)),
	!. % si P pair, alors c'est au minimizer de jouer
setJoueur(P):-
	parite(P),
	maximizer(rouge),
	joueurCourant(rouge),
	retract(joueurCourant(X)),
	assert(joueurCourant(jaune)),
	!.
setJoueur(P):-
	not(parite(P)),
	maximizer(rouge),
	joueurCourant(jaune),
	retract(joueurCourant(X)),
	assert(joueurCourant(rouge)),
	!. % P impair, maximizer joue
setJoueur(P):-
	not(parite(P)),
	maximizer(jaune),
	joueurCourant(rouge),
	retract(joueurCourant(X)),
	assert(joueurCourant(jaune)).
setJoueur(P).

evaluate(X):-
	X is random(10).

choixValeurNoeud(L,R,Value):-
	joueurCourant(X),
	maximizer(X),
	nl,write('maximizer algo '), write(X),
	coupAJouerMaximizer(L,R,Value),
	!.
choixValeurNoeud(L,R,Value):-
	nl,write('minimizer algo'),
	coupAJouerMinimizer(L,R,Value).

coupAJouerMaximizer(L, R,X):-
	membreMaxRank(X,L,R).
coupAJouerMinimizer(L, R,X):-
	membreMinRank(X,L,R).

parite(X):-
	divmod(X ,2, Q, R),
	R==0.

changerJoueur:-
	joueurCourant(jaune),
	retract(joueurCourant(X)),
	assert(joueurCourant(rouge)).
changerJoueur:-
	joueurCourant(rouge),
	retract(joueurCourant(X)),
	assert(joueurCourant(jaune)).

membreMaxRank(X, [Y|L], R):-
	membreMax(L, 1, 1, R, Y, X),!.

membreMax([], R, Rm, Rm, Max, Max).		   
membreMax([Y|L], R1, Rm, R, Max, X):-
	incr(R1,R2),
	maximum(Y, Max, Rm, R2, NewMax, NewRankMax),
	membreMax(L,R2,NewRankMax,R, NewMax, X).

membreMinRank(X, [Y|L], R):-
	membreMin(L, 1, 1, R, Y, X),!.

membreMin([], R, Rm, Rm, Max, Max).		   
membreMin([Y|L], R1, Rm, R, Max, X):-
	incr(R1,R2),
	minimum(Y, Max, Rm, R2, NewMax, NewRankMax),
	membreMin(L,R2,NewRankMax,R, NewMax, X).

minimum(Y, Max, OldRankMax, NewRankMax, Y, NewRankMax):-
	Y<Max.
minimum(Y, Max,OldRankMax, NewRankMax, Max, OldRankMax).  

maximum(Y, Max, OldRankMax, NewRankMax, Y, NewRankMax):-
	Y>Max.
maximum(Y, Max,OldRankMax, NewRankMax, Max, OldRankMax).  