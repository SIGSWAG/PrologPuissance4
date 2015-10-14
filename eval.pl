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

% evalPosition/2 (+Courant,-Score)
% Evalue en privilegiant les positions centrales
% Toujours vrai
evalPosition(Courant,Score) :- findall(S, evalCases(Courant,S), Scores), sum(Scores, Score).

evalCases(Courant,ScoreCase) :- case(X,Y,_), evalCase(X,Y,Courant,ScoreCase). % basculer vers caseTest !!!!!!!!!!!!

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