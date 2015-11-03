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

evalJeu(JoueurCourant,_,X,Y,Score):-
	gagneTest(X,Y,JoueurCourant,V),
	donneScore(JoueurCourant,V,Score).
	
donneScore(J,V,S):-
	infiniteNeg(V,S).
	
evalJeu(JoueurCourant,AutreJoueur,X,Y,Score) :-
	evalPosition(JoueurCourant,Score1),
	%% evalPuissances3(JoueurCourant,AutreJoueur,Score2),
	densite(JoueurCourant,Score3),
	evalAdjacence(X,Y,Joueur,Score4),
	Score2=0,
	Score is Score1*10+Score2+Score3*15+Score4.

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
	caseVideTest(X,Y), !.
ponderationJ(_, _, _, -1).

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
	assert(caseTest(X,Y,Joueur)),
	(gagneTest(X,Y,Joueur,1) -> ScoreCase = 20 ; ScoreCase = 0),
	retract(caseTest(X,Y,Joueur)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR ADJACENCE
%		  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evalAdjacence (+X,+Y,+Joueur,-Note)
% Donne une note plus forte si le pion courant
%est entoure de pions amis
% Toujours vrai

evalAdjacence(X,Y,Joueur,Note) :- aggregate_all(count,caseAdjacente(X,Y,Joueur,_,_),N), pow(N,2,Note).

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
% Direction (avec l ancien au milieu)
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
%#  caseTest(1,2,rouge).
%#  caseTest(1,3,rouge).
%#  caseTest(1,4,rouge).

%#  caseTest(2,1,rouge).
%#  caseTest(2,2,jaune).
%#  caseTest(2,3,rouge).
%#  caseTest(2,4,jaune).
%#
%#  caseTest(3,1,jaune).
%#  caseTest(3,2,rouge).
%#  caseTest(3,3,jaune).
%#  caseTest(3,4,rouge).

%#  caseTest(4,1,rouge).
%#  caseTest(4,2,rouge).
%#  caseTest(4,3,jaune).
%#  caseTest(4,4,jaune).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


gagneTest(X,Y,J,V) :- %V=1 si victoire direct, 0 si indirect
	gagneColonneTest(X,Y,J,V).
gagneTest(X,Y,J,V) :-
	gagneLigneTest(X,Y,J,V).
gagneTest(X,Y,J,V) :-
	gagneDiag1Test(X,Y,J,V).
gagneTest(X,Y,J,V) :-
	gagneDiag2Test(X,Y,J,V).
	
	
testPotentielAccumulation(X,Y,J,P,A):-
	nbColonnes(NBCOLONNES), X=<NBCOLONNES, X>=1,
	caseVideTest(X,Y), %Case vide
	gagneTestDirect(X,Y,J), %On gagne si on pose dessus
	testPotentiel(X,Y,J,P), %Peut on la remplir au prochain coup?
	testAccumulation(X,Y,J,A). %As-t-on accumulation?

testPotentiel(X,1,J,1).	%case au niveau 1
testPotentiel(X,Y,J,1):-
	decr(Y,Y1),
	caseTest(X,Y1,_).  %On peut la remplir
testPotentiel(X,Y,J,0). %On ne peut pas la remplir


testAccumulation(X,Y,J,1) :- incr(Y,Y1), caseVideTest(X,Y1), gagneTestDirect(X,Y1,J). %Case au dessus gagnante aussi
testAccumulation(X,Y,J,1) :- decr(Y,Y1), caseVideTest(X,Y1), gagneTestDirect(X,Y1,J). %Case en dessous gagnante aussi
testAccumulation(X,Y,J,0). %Pas d'accumulation.
	
	
testFinal(2,A,0).
testFinal(P,A,-1):-
	A > 0.
	

gagneColonneTest(X,Y,J,-1) :-
	nbLignes(L),
	Y<L,
	decr(Y,Y1),
	caseTest(X,Y1,J),
	decr(Y1,Y2),
	caseTest(X,Y2,J), %3 d'affilée
	incr(Y,Y3), 
	incr(Y3,Y4),
	gagneTestDirect(X,Y4,J). %Accumulation 
	

gagneLigneTest(X,Y,J,V) :-
	decr(X,X1),
	gaucheVerifTest(X1,Y,J,P1,A1),
	incr(X,X2),
	droiteVerifTest(X2,Y,J,P2,A2),
	!, 
	Pfin is P1+P2, 
	Afin is A1+A2,
	testFinal(Pfin,Afin,V).


gaucheVerifTest(X,Y,J,Pg,A):- %Pg cases libres sur les c�t�s (2 max)
	gaucheTest(X,Y,J,Pg,A).
	
gaucheTest(X,Y,J,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A). %Faux seulement si case de l'autre couleur ou si coup dépasse du tableau
gaucheTest(X,Y,J,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur � gauche ou fin du tableau
gaucheTest(X,Y,J,P,A) :-
	decr(X,X1),
	gaucheTest(X1,Y,J,P,A).

droiteVerifTest(X,Y,J,Pg,A):-
	droiteTest(X,Y,J,Pg,A).
	
droiteTest(X,Y,J,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A). %Faux seulement si case de l'autre couleur ou si coup dépasse du tableau
droiteTest(X,Y,J,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur � droite 
droiteTest(X,Y,J,P,A) :-
	incr(X,X1),
	droiteTest(X1,Y,J,P,A).

gagneDiag1Test(X,Y,J,V) :-
	incr(Y,Y1),
	decr(X,X1),
	gaucheHautVerifTest(X1,Y1,J,P1,A1),
	decr(Y,Y2),
	incr(X,X2),
	droiteBasVerifTest(X2,Y2,J,P2,A2),
	!,
	 Pfin is P1+P2, Afin is A1+A2,
	testFinal(Pfin,Afin,V).

gaucheHautVerifTest(X,Y,J,Pg,A):-
	gaucheHautTest(X,Y,J,Pg,A).
gaucheHautTest(X,Y,J,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A).  
gaucheHautTest(X,Y,J,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur en bas � gauche
gaucheHautTest(X,Y,J,P,A) :-
	incr(Y,Y1),
	decr(X,X1),
	gaucheHautTest(X1,Y1,J,P,A).


droiteBasVerifTest(X,Y,J,Pg,A):-
	droiteBasTest(X,Y,J,Pg,A).
droiteBasTest(X,Y,J,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A). 
droiteBasTest(X,Y,J,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur en bas � droite 
droiteBasTest(X,Y,J,P,A) :-
	decr(Y,Y1),
	incr(X,X1),
	droiteBasTest(X1,Y1,J,P,A).

gagneDiag2Test(X,Y,J,V) :-
	decr(Y,Y1),
	decr(X,X1),
	gaucheBasVerifTest(X1,Y1,J,P1,A1),
	incr(Y,Y2),
	incr(X,X2),
	droiteHautVerifTest(X2,Y2,J,P2,A2),
	!,
	 Pfin is P1+P2, Afin is A1+A2,
	testFinal(Pfin,Afin,V).

gaucheBasVerifTest(X,Y,J,Pg,A) :-
	gaucheBasTest(X,Y,J,Pg,A).
gaucheBasTest(X,Y,J,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A).
gaucheBasTest(X,Y,J,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur en bas � gauche
gaucheBasTest(X,Y,J,P,A) :-
	decr(Y,Y1),
	decr(X,X1),
	gaucheBasTest(X1,Y1,J,P,A).


droiteHautVerifTest(X,Y,J,Pg,A) :-
	droiteHautTest(X,Y,J,Pg,A).
droiteHautTest(X,Y,J,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A). 
droiteHautTest(X,Y,J,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur en haut � droite 
droiteHautTest(X,Y,J,P,A) :-
	incr(Y,Y1),
	incr(X,X1),
	droiteHautTest(X1,Y1,J,P,A).

	
	
%%%%% gagneTestDirect %%%%%




gagneTestDirect(X,Y,J) :-
	gagneTestDirectLigne(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag1(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag2(X,Y,J).
	

%%% En ligne %%%

gagneTestDirectLigne(X,Y,J) :-
	decr(X,X1),
	gaucheVerif(X1,Y,J,Rg),
	incr(X,X2),
	droiteVerif(X,Y2,J,Rd),
	!,
	Rf is Rg+Rd, Rf>2.

gaucheVerif(X,Y,J,Rg):-
	gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gauche(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):-
	droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droite(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droite(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneTestDirectDiag1(X,Y,J) :-
	decr(X,X1),
	incr(Y,Y1),
	gaucheHautVerif(X1,Y1,J,Rg),
	incr(X,X2),
	decr(Y,Y2),
	droiteBasVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheHautVerif(X,Y,J,Rg):-
	gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):-
	droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneTestDirectDiag2(X,Y,J) :-
	decr(X,X1),
	decr(Y,Y1),
	gaucheBasVerif(X1,Y1,J,Rg),
	incr(X,X2),
	incr(Y,Y2),
	droiteHautVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheBasVerif(X,Y,J,Rg) :-
	gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg) :-
	droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHaut(X1,Y1,J,R1,Rg).
	
	
%%%%%%% caseVideTest %%%%%

caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).
	
