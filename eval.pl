%%%%%%%%%%%% eval.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- module(eval, [evalJeu/3]).

:- use_module(util).

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% evalJeu/3(+JoueurCourant, +AutreJoueur, -Score)
% Evalue la situation courante.
% Score s'unifie avec le score évalué pour la position courante.
%evalJeu(JoueurCourant,AutreJoueur,Score) :- evalPuissances3(JoueurCourant,AutreJoueur,Score).
evalJeu(_,_,1).

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

% TODO TREEEES IMPORTANT :
% TOUT BASCULER VERS CASETEST DES QUE MINMAX EST PRET !!!!!!!!!!!!

% evalPosition/2 (+Courant,-Score)
% Evalue en privilegiant les positions centrales
% Toujours vrai
evalPosition(Courant,Score) :- findall(S, evalCases(Courant,S), Scores), sum(Scores, Score).

evalCases(Courant,ScoreCase) :- case(X,Y,_), evalCase(X,Y,Courant,ScoreCase).

evalCase(X,Y,Courant,ScoreCase) :- 
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	ponderationJ(X, Y, Courant, PonderationJoueur),
	CentreX is NBCOLONNES // 2 + 1, CentreY is NBLIGNES // 2 + 1,
	Dx is X - CentreX, Dy is Y - CentreY,
	abs(Dx,AbsX),abs(Dy,AbsY),
	ScoreCase is ( 100/(AbsX+1) + 100/(AbsY+1) )*PonderationJoueur.

ponderationJ(X, Y, Courant, 1) :- case(X,Y,Courant), !.
ponderationJ(X, Y, _, 0) :- caseVide(X,Y), !.
ponderationJ(_, _, _, -1).

%%%%%%%%%%%%%%%%%%%%

% evalPuissances3/3(+JoueurCourant,+AutreJoueur,-Score)
% Évalue en cherchant les positions faisant gagner.
% Score s'unifie au score de la position.
evalPuissances3(JoueurCourant,AutreJoueur,ScoreFinal) :-
	findall(S,evalCasesVides(JoueurCourant,S),ScoresCourant), sum(ScoresCourant,ScoreCourant),
	findall(S,evalCasesVides(AutreJoueur,S),ScoresAutre), sum(ScoresAutre,ScoreAutre),
	ScoreFinal is ScoreCourant - ScoreAutre.

evalCasesVides(Joueur,ScoreCase) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	between(1,NBCOLONNES,X), between(1,NBLIGNES,Y),
	caseVide(X,Y),
	assert(jeu:case(X,Y,Joueur)),
	(gagne(X,Y,Joueur) -> ScoreCase = 1 ; ScoreCase = 0),
	retract(jeu:case(X,Y,Joueur)).





sum([],0).
sum([X|Xs],N) :- sum(Xs,N1), N is N1+X.

% evalAdjacence/2 (+Courant,-Score)
% Evalue en privilegiant les cases entourees par des positions amies
% -
evalAdjacence(Courant,Score) :- findall(S, evalCases(Courant,S), Scores), sum(Scores, Score).


evalCasesAdjacentes(X,Y,Courant,ScoreCase) :- SC0 is 0, decr(X,X1), incr(Y,Y1), decr(X,X2), incr(Y,Y2),
	ponderationJ(X1,Y,Courant, SC0), sum(SC0,SC1), 
	ponderationJ(X1,Y1,Courant, SC1), sum(SC1,SC2),
	ponderationJ(X,Y1,Courant, SC2), sum(SC2,SC3),
	ponderationJ(X2,Y1,Courant, SC3), sum(SC3,SC4),
	ponderationJ(X2,Y,Courant, SC4), sum(SC4,SC5),
	ponderationJ(X2,Y2,Courant, SC5), sum(SC5,SC6),
	ponderationJ(X,Y2,Courant, SC6), sum(SC6,SC7),
	ponderationJ(X1,Y2,Courant, SC7), sum(SC7,ScoreCase).