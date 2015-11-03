%%%%%%%%%%%% eval.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- module(eval, [evalJeu/5,evalTest1/2]).

:- use_module(util).
:- use_module(jeu).
:- use_module(miniMax). % pour caseTest, peut-être à changer
:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

evalTest1(1,-3).
evalTest1(2,-4).
evalTest1(3,5).
evalTest1(4,10).
evalTest1(5,9).
evalTest1(6,-5).
evalTest1(7,8).


% evalJeu/5(+JoueurCourant, +AutreJoueur, +X, +Y, -Score)
% Evalue la situation courante pour le joueur JoueurCourant étant donné que le dernier coup joué fut joué en (X,Y).
% Score s unifie avec le score évalué pour la position courante.
	
evalJeu(JoueurCourant,AutreJoueur,X,Y,Score) :-
	assert(caseTest(X,Y,JoueurCourant)),
	assert(ennemiTest(AutreJoueur)),
	%%evalPosition(JoueurCourant,Score1),
	evalPuissances3(JoueurCourant,AutreJoueur,Score2),
	%% densite(JoueurCourant,Score3),
	retract(caseTest(X,Y,JoueurCourant)),
	retract(ennemiTest(AutreJoueur)),
	Score1=0,
	Score3=0,
	random_between(-2,2,Perturbation),
	Score is Score1+Score2+Score3+Perturbation.

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

% evalPosition/2 (+Courant,-Score)
% Evalue en privilégiant les positions centrales.
% renvoie un score entre -400 et 400
% Toujours vrai.
evalPosition(Courant,Score) :-
	assert(nbCasesPleines(0)),
	findall(S, evalCases(Courant,S), Scores),
	sum(Scores, ScoreTot),
	nbCasesPleines(NbCasesPleinesFinal),
	retract(nbCasesPleines(NbCasesPleinesFinal)),
	Score is ScoreTot / NbCasesPleinesFinal.

evalCases(Courant,ScoreCase) :-
	caseTest(X,Y,_),
	nbCasesPleines(NbCasesPleines),
	retract(nbCasesPleines(NbCasesPleines)),
	incr(NbCasesPleines,NbCasesPleinesF),
	assert(nbCasesPleines(NbCasesPleinesF)),
	evalCase(X,Y,Courant,ScoreCase).

% renvoie un score entre -400 et 400
evalCase(X,Y,Courant,ScoreCase) :-
	nbColonnes(NBCOLONNES),
	nbLignes(NBLIGNES),
	ponderationJ(X, Y, Courant, PonderationJoueur),
	CentreX is NBCOLONNES // 2 + 1,
	CentreY is NBLIGNES // 2 + 1,
	Dx is X - CentreX,
	Dy is Y - CentreY,
	abs(Dx,AbsX),
	abs(Dy,AbsY),
	ScoreCase is ( 200/(AbsX+1) + 200/(AbsY+1) )*PonderationJoueur.

ponderationJ(X,Y, Courant,1) :-
	caseTest(X,Y,Courant), !.
ponderationJ(X,Y,Courant,-1) :-
	ennemiTest(J),
	caseTest(X,Y,J), !.
ponderationJ(_,_,_,0).

%%%%%%%%%%%%%%%%%%%%

% evalPuissances3/3(+JoueurCourant,+AutreJoueur,-Score)
% Évalue en cherchant les positions faisant gagner.
% ScoreFinal s unifie au score de la position.
evalPuissances3(JoueurCourant,AutreJoueur,ScoreFinal) :-
	findall(S,evalCasesVides(JoueurCourant,S),ScoresCourant), sum(ScoresCourant,ScoreCourant),
	findall(S,evalCasesVides(AutreJoueur,S),ScoresAutre), sum(ScoresAutre,ScoreAutre),
	ScoreFinal is ScoreCourant - ScoreAutre.

evalCasesVides(Joueur,ScoreCase) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	between(1,NBCOLONNES,X), between(1,NBLIGNES,Y),
	caseVideTest(X,Y),
	aDesVoisins(X,Y,Joueur),
	assert(caseTest(X,Y,Joueur)),
	(gagneTest(X,Y,Joueur,1) -> ScoreCase = 100 ; ScoreCase = 0),
	retract(caseTest(X,Y,Joueur)).

% vrai si la case X,Y a des voisins rempli (non vides)
aDesVoisins(X,Y,Joueur) :- incr(X,X1),caseTest(X1,Y,Joueur).
aDesVoisins(X,Y,Joueur) :- incr(Y,Y1),caseTest(X,Y1,Joueur).
aDesVoisins(X,Y,Joueur) :- decr(X,X1),caseTest(X1,Y,Joueur).
aDesVoisins(X,Y,Joueur) :- decr(Y,Y1),caseTest(X,Y1,Joueur).
aDesVoisins(X,Y,Joueur) :- incr(X,X1),incr(Y,Y1),caseTest(X1,Y1,Joueur).
aDesVoisins(X,Y,Joueur) :- decr(X,X1),decr(Y,Y1),caseTest(X1,Y1,Joueur).
aDesVoisins(X,Y,Joueur) :- incr(X,X1),decr(Y,Y1),caseTest(X1,Y1,Joueur).
aDesVoisins(X,Y,Joueur) :- decr(X,X1),incr(Y,Y1),caseTest(X1,Y1,Joueur).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR DENSITE DE PION ~
%		  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% densite (+Joueur,-Note)
% Donne une note d autant plus elevee que les pions sont groupes
% Toujours vrai
densite(J,Note) :- Z is 1, calculNbPoints(J,Z,Note).
calculNbPoints(J,Z,Note) :- Z>6, Note is 0.
calculNbPoints(J,Z,Note) :- nbPointsZone(J,Z,N), incr(Z,ZP), calculNbPoints(J,ZP,NP), Note is N+NP.
nbPointsZone(J,Z,NbPoints) :- nbPionsZone(J,Z,N), pow(N,2,NbPoints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nbPionsZone (+joueur,+zone,-nbPions)
% Donne le nombre de pions contenu dans une zone
% Toujours vrai
nbPionsZone(J,Z,NbPions) :- aggregate_all(count,caseTestZone(Z,J,X,Y),NbPions).

caseTestZone(Zone,Joueur,X,Y) :- caseTest(X,Y,Joueur), zone(Zone,X,Y).
zone(1,X,Y) :- X =<3, Y =< 3.
zone(2,X,Y) :- X = 4, Y =< 3.
zone(3,X,Y) :- X > 4, Y =< 3.
zone(4,X,Y) :- X > 4, Y > 3.
zone(5,X,Y) :- X = 4, Y > 3.
zone(6,X,Y) :- X =<3, Y > 3.

%%%%%%% caseVideTest %%%%%
% caseVideTest(+X,+Y)
% vrai si la case X,Y est vide
caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).
	
