%%%%%%%%%%%% eval.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- module(eval, [evalJeu/2]).

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%
% evalJeu/2 (+Courant, -Score)
% Evalue la situation Courante.
% Tout le temps vrai.
evalJeu(Courant,Score) :- evalPosition(Courant,Score).

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
ponderationJ(_, _, _, -1).
	
sum([],0).
sum([X|Xs],N) :- sum(Xs,N1), N is N1+X.

%%%%%%%%%%%%%%%%%%%%

% evalPuissances3/2(+JoueurCourant,-Score)
% Évalue en cherchant les positions faisant gagner.
% Score s'unifie au score de la position.
evalPuissances3(JoueurCourant,ScoreFinal) :- 
	findall(S,evalCasesVides(JoueurCourant,S),ScoresCourant), sum(ScoresCourant,ScoreCourant),
	findall(S,evalCasesVides(AutreJoueur,S),ScoresAutre), sum(ScoresAutre,ScoreAutre),
	ScoreFinal is ScoreCourant + ScoreAutre.

evalCasesVides(JoueurCourant,ScoreCase) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	between(1,NBCOLONNES,X), between(1,NBLIGNES,Y),
	caseVide(X,Y),
	assert(case(X,Y,JoueurCourant)),
	ScoreCase = (gagne(X,Y,JoueurCourant) -> 1 ; -1),
	retract(case(X,Y,JoueurCourant).