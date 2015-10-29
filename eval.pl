%%%%%%%%%%%% eval.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- module(eval, [evalJeu/5]).

:- use_module(util).
:- use_module(jeu).
:- use_module(miniMax). % pour caseTest, peut-être à changer

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% evalJeu/5(+JoueurCourant, +AutreJoueur, +X, +Y, -Score)
% Evalue la situation courante pour le joueur JoueurCourant étant donné que le dernier coup joué fut joué en (X,Y).
% Score s'unifie avec le score évalué pour la position courante.

%evalJeu(JoueurCourant,_,X,Y,Score) :-
%	gagneTest(X,Y,JoueurCourant),
%	infinitePos(Score).
%evalJeu(_,AutreJoueur,X,Y,Score) :-
%	gagneTest(X,Y,AutreJoueur),
%	infiniteNeg(Score).

evalJeu(JoueurCourant,_,_,_,Score) :-
	evalPosition(JoueurCourant,Score).
	%evalPuissances3(JoueurCourant,AutreJoueur,Score).
	%evalAdjacence(JoueurCourant,Score).

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

% evalPosition/2 (+Courant,-Score)
% Evalue en privilégiant les positions centrales.
% Toujours vrai.
evalPosition(Courant,Score) :-
	findall(S, evalCases(Courant,S), Scores),
	sum(Scores, Score).

evalCases(Courant,ScoreCase) :-
	caseTest(X,Y,_),
	evalCase(X,Y,Courant,ScoreCase).

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
	ScoreCase is ( 100/(AbsX+1) + 100/(AbsY+1) )*PonderationJoueur.

ponderationJ(X, Y, Courant, 1) :-
	caseTest(X,Y,Courant), !.
ponderationJ(X, Y, _, 0) :-
	caseVide(X,Y), !.
ponderationJ(_, _, _, -1).

%%%%%%%%%%%%%%%%%%%%

% evalPuissances3/3(+JoueurCourant,+AutreJoueur,-Score)
% Évalue en cherchant les positions faisant gagner.
% ScoreFinal s'unifie au score de la position.
evalPuissances3(JoueurCourant,AutreJoueur,ScoreFinal) :-
	findall(S,evalCasesVides(JoueurCourant,S),ScoresCourant), sum(ScoresCourant,ScoreCourant),
	findall(S,evalCasesVides(AutreJoueur,S),ScoresAutre), sum(ScoresAutre,ScoreAutre),
	ScoreFinal is ScoreCourant - ScoreAutre.

evalCasesVides(Joueur,ScoreCase) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	between(1,NBCOLONNES,X), between(1,NBLIGNES,Y),
	caseVide(X,Y),
	assert(caseTest(X,Y,Joueur)),
	(gagneTest(X,Y,Joueur) -> ScoreCase = 1 ; ScoreCase = 0),
	retract(caseTest(X,Y,Joueur)).

% --------- DEPRECATED ------------
% evalAdjacence/2 (+Courant,-Score)
% Evalue en privilegiant les cases entourees par des positions amies
% -
evalAdjacence(Courant,Score) :-
	findall(S, evalCases(Courant,S), Scores),
	sum(Scores, Score).


evalCasesAdjacentes(X,Y,Courant,ScoreCase) :-
	SC0 is 0,
	decr(X,X1),
	incr(Y,Y1),
	decr(X,X2),
	incr(Y,Y2),
	ponderationJ(X1,Y,Courant, SC0), sum(SC0,SC1), 
	ponderationJ(X1,Y1,Courant, SC1), sum(SC1,SC2),
	ponderationJ(X,Y1,Courant, SC2), sum(SC2,SC3),
	ponderationJ(X2,Y1,Courant, SC3), sum(SC3,SC4),
	ponderationJ(X2,Y,Courant, SC4), sum(SC4,SC5),
	ponderationJ(X2,Y2,Courant, SC5), sum(SC5,SC6),
	ponderationJ(X,Y2,Courant, SC6), sum(SC6,SC7),
	ponderationJ(X1,Y2,Courant, SC7), sum(SC7,ScoreCase).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			DETERMINATION COUP GAGNANT (BIS)
%		  RECHERCHE D EXTREMITES DE SEQUENCES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% coupGagnant (+X,+Y,+joueur)
% Determine si le coup joue immediatement avant est gagnant ou non en fonction du pion (X,Y) joue
% Vrai pour un coup gagnant, faux sinon

coupGagnant(X,Y,Joueur)	:- rechercheExtremite(X,Y,Xextr,Yextr,1,Joueur), pionsSuccessifs(Xextr,Yextr,Joueur,5, Npions), Npions > 3.
coupGagnant(X,Y,Joueur) :- rechercheExtremite(X,Y,Xextr,Yextr,2,Joueur), pionsSuccessifs(Xextr,Yextr,Joueur,6, Npions), Npions > 3.
coupGagnant(X,Y,Joueur)	:- rechercheExtremite(X,Y,Xextr,Yextr,3,Joueur), pionsSuccessifs(Xextr,Yextr,Joueur,7, Npions), Npions > 3.
coupGagnant(X,Y,Joueur) :- rechercheExtremite(X,Y,Xextr,Yextr,4,Joueur), pionsSuccessifs(Xextr,Yextr,Joueur,8, Npions), Npions > 3.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pionsSuccessifs (+X,+Y,+joueur,+direction,?nbPionsSuccessifs).
% Donne le nombre de pions successifs trouves dans une direction donnee (de 1 a 8)
% Toujours vrai

pionsSuccessifs(X,Y,Joueur,Direction,1) :- nwCoord(X,Y,Direction,Xn,Yn),not(caseTest(Xn,Yn,Joueur)).
pionsSuccessifs(X,Y,Joueur,Direction,NbPionsTrouves) :- nwCoord(X,Y,Direction,Xn,Yn),caseTest(Xn,Yn,Joueur),
														pionsSuccessifs(Xn,Yn,Joueur,Direction,NbP),
														incr(NbP,NbPionsTrouves).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rechercheExtremite(+Xcourant,+Ycourant,-Xextremite,-Yextremite,+Direction,+Joueur)
% Donne en fonction d un point de depart Xc,Yc, la position du dernier pion de la couleur du joueur dans une direction
% Toujours vrai

rechercheExtremite(Xextr,Yextr,Xextr,Yextr,Dir,Joueur) :- nwCoord(Xextr,Yextr,Dir,Xh,Yh),not(caseTest(Xh,Yh,Joueur)).
rechercheExtremite(Xcurr,Ycurr,Xextr,Yextr,Dir,Joueur) :- nwCoord(Xcurr,Ycurr,Dir,Xn,Yn),caseTest(Xn,Yn,Joueur),rechercheExtremite(Xn,Yn,Xextr,Yextr,Dir,Joueur).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nwCoord (+Xold,+Yold,+Direction,-Xnew,-Ynew)
% Donne les nouvelles coordonnees en fonction de la Direction
% Direction (avec l'ancien au milieu)
%	- - - - -
%	- 4 3 2 -
% 	- 5 X 1 -
%	- 6 7 8 -
%	- - - - -

nwCoord(X,Y,1,Xn,Yn) :- Yn is Y, incr(X,Xn).
nwCoord(X,Y,2,Xn,Yn) :- incr(X,Xn), incr(Y,Yn).
nwCoord(X,Y,3,Xn,Yn) :- Xn is X, incr(Y,Yn).
nwCoord(X,Y,4,Xn,Yn) :- decr(X,Xn), incr(Y,Yn).
nwCoord(X,Y,5,Xn,Yn) :- Yn is Y, decr(X,Xn).
nwCoord(X,Y,6,Xn,Yn) :- decr(X,Xn), decr(Y,Yn).
nwCoord(X,Y,7,Xn,Yn) :- Xn is X, decr(Y,Yn).
nwCoord(X,Y,8,Xn,Yn) :- incr(X,Xn), decr(Y,Yn).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% invertDirection(+oldDir,-newDir)
% Inverse la direction en chargeant dans newDir la nouvelle direction
% Toujours vrai
%invertDirection(Od,Nd) :- Nd is Od+(4^(-Od/5)).

invertDirection(Od,Nd) :- Od < 5, Nd is Od+4.
invertDirection(Od,Nd) :- Od >=5, Nd is Od-4.

%# caseTest(1,1,jaune).
%# caseTest(1,2,rouge).
%# caseTest(1,3,rouge).
%# caseTest(1,4,rouge).

%# caseTest(2,1,rouge).
%# caseTest(2,2,jaune).
%# caseTest(2,3,rouge).
%# caseTest(2,4,jaune).

%# caseTest(3,1,jaune).
%# caseTest(3,2,rouge).
%# caseTest(3,3,jaune).
%# caseTest(3,4,rouge).

%# caseTest(4,1,rouge).
%# caseTest(4,2,rouge).
%# caseTest(4,3,jaune).
%# caseTest(4,4,jaune).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caseAdjacente(+XcaseActuelle,+YcaseActuelle,
%				?CouleurCaseAdjacente,
%				-XcaseAdjacente,-YcaseAdjacente)
% Donne toutes les cases adjacentes a la case envoyee en coordonnees
% Vrai si il existe des cases adjacentes

caseAdjacente(X,Y,J,Xn,Yn) :- nwCoord(X,Y,_,Xn,Yn),caseTest(Xn,Yn,J).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caseAmie(+X,+Y,-Xn,-Yn)
% Cases amies autour de la case envoyee en param
% Vrai si il existe des cases amies

caseAmie(X,Y,Xn,Yn) :- caseTest(X,Y,J), caseAdjacente(X,Y,J,Xn,Yn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caseEnnemie(+X,+Y,-Xn,-Yn)
% Cases amies autour de la case envoyee en param
% Vrai si il existe des cases ennemies

caseEnnemie(X,Y,Xn,Yn) :- caseTest(X,Y,J), ennemi(J,E), caseAdjacente(X,Y,E,Xn,Yn).
