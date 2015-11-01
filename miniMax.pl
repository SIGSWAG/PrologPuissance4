%%%%%%%%%%%% miniMax.pl %%%%%%%%%%%%

:- module(miniMax, [parcoursArbre/4, caseTest/3]).

:- use_module(util).
:- use_module(eval).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For test purpose, this module contains duplicate code.%%
%% do not remove it.									 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic caseTest/3.
:- dynamic cestMort/1.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% +J player qui doit jouer
% +Pmax prof maximale
% -R le coup a jouer
% -Value évaluation du noeud courant
parcoursArbre(J,Pmax,R,Value):- 
	initCaseTest,infinitePos(InfP),infiniteNeg(InfN),assert(maximizer(J)), assert(joueurCourant(J)), 
	parcours(1,1,Pmax,[1,0],InfP,InfN), feuille([1,0],X1), 
	setJoueur(1), parcours(2,1,Pmax,[2,0],InfP,X1), feuille([2,0],X2),
	setJoueur(1), AlphaNext is max(X1,X2), parcours(3,1,Pmax,[3,0],InfP,AlphaNext), feuille([3,0],X3), 
	setJoueur(1), AlphaNext1 is max(AlphaNext,X3), parcours(4,1,Pmax,[4,0],InfP,AlphaNext1), feuille([4,0],X4), 
	setJoueur(1), AlphaNext2 is max(AlphaNext1,X4), parcours(5,1,Pmax,[5,0],InfP,AlphaNext2), feuille([5,0],X5), 
	setJoueur(1), AlphaNext3 is max(AlphaNext2,X5), parcours(6,1,Pmax,[6,0],InfP,AlphaNext3), feuille([6,0],X6), 
	setJoueur(1), AlphaNext4 is max(AlphaNext3,X6), parcours(7,1,Pmax,[7,0],InfP,AlphaNext4), feuille([7,0],X7), 
	coupAJouerMaximizer([X1,X2,X3,X4,X5,X6,X7],R,Value), clearTest,!. %the second call and the next ones are called with the result of the preceding (we take the max of all of them) on reset le joueur entre chaque call


%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

initCaseTest:- case(X,Y,Z), assert(caseTest(X,Y,Z)), false. %on assert une caseTest pour toutes les cases.
initCaseTest.

clearTest:- 
	retractall(caseTest(X,Y,_)), 
	%retractall(feuille(X,Y)), 
	retract(maximizer(X)), retract(joueurCourant(_)). % on eve tout ce que l'on a ajouté.

%Parcours
%+X la colonne a jouer
%+P la profondeur courante
%+L la liste de coups courante
%+Beta,+Alpha les valeurs courantes pour Alpha et Beta.


parcours(X, _, _, L, _, _):- nbLignes(MaxLignes),jeu:case(X,MaxLignes,_), joueurCourant(Joue), maximizer(Joue), infiniteNeg(2,Value), assert(feuille(L, Value)). % on ne peut PAS jouer, on met -infini
parcours(X, _, _, L, _, _):- nbLignes(MaxLignes),jeu:case(X,MaxLignes,_), joueurCourant(Joue), not(maximizer(Joue)), infinitePos(2,Value), assert(feuille(L, Value)). % on ne peut PAS jouer, on met +infini
parcours(X, _, _, L, _, _):- nbLignes(MaxLignes),caseTest(X,MaxLignes,_), joueurCourant(Joue), evaluate(X,MaxLignes,Joue,Value), assert(feuille(L, Value)) .% on ne peut plus jouer, on met une feuille (on évalue)


parcours(X, _, _, L, _, _):-  joueurCourant(Joue), calculPositionJeton(X, 1, Y), gagneTest(X,Y,Joue,Direct), maximizer(Joue), infinitePos(Direct,Value), assert(feuille(L, Value)),retract(caseTest(X,Y,Joue)). %Victoire du max
parcours(X, _, _, L, _, _):-  joueurCourant(Joue), calculPositionJeton(X, 1, Y), gagneTest(X,Y,Joue,Direct), not(maximizer(Joue)), infiniteNeg(Direct,Value), assert(feuille(L, Value)),retract(caseTest(X,Y,Joue)). %Victoire du min

parcours(X, P, Pmax, L, _, _):- P==Pmax,joueurCourant(Joue), placerJeton(X,Y,Joue), evaluate(X, Y, Joue, Value),assert(feuille(L, Value)),retract(caseTest(X,Y,Joue)). % on est à la prof max, on evalue et on met une feuille
parcours(X, P, Pmax, L, Beta, Alpha) :- incr(P, P1),joueurCourant(Joue), placerJeton(X,Y,Joue), %on incremente la profondeur, puis on joue un coup(qui réussit a tous les coups)
	setJoueur(P1), %on set le joueur
	attribueVal(ValeurPrec), % on initialise val
	parcours(1, P1,Pmax, [1|L], Beta, Alpha), %on joue colonne 1
	feuille([1|L], Valeur1),%here is the value of first branch

	setJoueur(P1), % on reset le joueur (il a changé dans le premier parcours)
	choixVal(Valeur1,ValeurPrec,Val1),%choisit si min ou max, renvoie la valeur pour le prochain coup.

	joueCoupSuivant(Val1,2,P1,Pmax,L,Beta,Alpha,Val2,Beta2,Alpha2),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), % on reset le joueur (il a changé dans le premier parcours)

	joueCoupSuivant(Val2,3,P1,Pmax,L,Beta2,Alpha2,Val3,Beta3,Alpha3),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	joueCoupSuivant(Val3,4,P1,Pmax,L,Beta3,Alpha3,Val4,Beta4,Alpha4),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	joueCoupSuivant(Val4,5,P1,Pmax,L,Beta4,Alpha4,Val5,Beta5,Alpha5),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	joueCoupSuivant(Val5,6,P1,Pmax,L,Beta5,Alpha5,Val6,Beta6,Alpha6),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	joueCoupSuivant(Val6,7,P1,Pmax,L,Beta6,Alpha6,Valeur,_,_),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	retract(caseTest(X,Y,Joue)), %on annule le coup pour poursuivre dans l'arbre
	feuille([1|L], _),feuille([2|L], _), %on cherche les feuilles associées (elles ont été calculées plus bas dans l'arbre)
	setJoueur(P1), %on change de joueur
	assert(feuille(L,Valeur)),joueurCourant(_). %on met notre feuille calculée

evaluate(X,Y,Joueur,Score) :-
	ennemi(Joueur,AutreJoueur),
	evalJeu(Joueur,AutreJoueur,X,Y,Score).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% élagage alpha beta% %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


choixVal(Valeur1,ValeurPrec,Val1):- joueurCourant(Joue), maximizer(Joue), Val1 is max(Valeur1, ValeurPrec). % we choose after the first choice if we take max or min for val
choixVal(Valeur1,ValeurPrec,Val1):- Val1 is min(Valeur1, ValeurPrec).

attribueVal(X):- infiniteNeg(InfN), joueurCourant(Joue), maximizer(Joue), X is InfN. % the initial value of a node (-inf if maximizer, +inf if minimizer)
attribueVal(X):-infinitePos(InfP), X is InfP.


%%%For the Minimizer
joueCoupSuivant(ValeurPrec,ColonneAJouer,_,_,L,Beta,Alpha,Val,Beta,Alpha):-joueurCourant(Joue), not(maximizer(Joue)), ValeurPrec =< Alpha, Val is ValeurPrec, assert(feuille([ColonneAJouer|L], Val)).%coupure alpha !!
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L,Beta,Alpha,Val,BetaCalc,Alpha):-joueurCourant(Joue), not(maximizer(Joue)), BetaCalc is min(Beta, ValeurPrec), parcours(ColonneAJouer, P1,Pmax,[ColonneAJouer|L],BetaCalc, Alpha), feuille([ColonneAJouer|L], ValeurFils), Val is min(ValeurFils, ValeurPrec). %pas de coupure!


%%For the Maximizer
joueCoupSuivant(ValeurPrec,ColonneAJouer,_,_,L, Beta, Alpha,Val,Beta,Alpha):-joueurCourant(Joue), maximizer(Joue), ValeurPrec >= Beta, Val is ValeurPrec,assert(feuille([ColonneAJouer|L], Val)).%coupure beta !!
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L, Beta, Alpha,Val,Beta,AlphaCalc):-joueurCourant(Joue), maximizer(Joue), AlphaCalc is max(Alpha, ValeurPrec), parcours(ColonneAJouer, P1,Pmax,[ColonneAJouer|L],Beta, AlphaCalc),feuille([ColonneAJouer|L], ValeurFils),Val is max(ValeurFils, ValeurPrec). %pas de coupure!





%%%%%%%%%% Aide calcul. Redundant code, do not remove %%%%%%%%%%%%%
setJoueur(P):- parite(P), maximizer(jaune), joueurCourant(jaune),retract(joueurCourant(_)), assert(joueurCourant(rouge)),!. % si P pair, alors c'est au minimizer de jouer
setJoueur(P):- parite(P), maximizer(rouge), joueurCourant(rouge),retract(joueurCourant(_)), assert(joueurCourant(jaune)),!.
setJoueur(P):- not(parite(P)),maximizer(rouge), joueurCourant(jaune),retract(joueurCourant(_)), assert(joueurCourant(rouge)),!. % P impair, maximizer joue
setJoueur(P):- not(parite(P)),maximizer(jaune), joueurCourant(rouge),retract(joueurCourant(_)), assert(joueurCourant(jaune)).
setJoueur(_).

choixValeurNoeud(L,R,Value):- joueurCourant(X), maximizer(X), coupAJouerMaximizer(L,R,Value),!. %on choisit val min ou max
choixValeurNoeud(L,R,Value):- coupAJouerMinimizer(L,R,Value).


coupAJouerMaximizer(L, R,X):- membreMaxRank(X,L,R).
coupAJouerMinimizer(L, R,X):- membreMinRank(X,L,R).


parite(X):- divmod(X ,2, _, 0).

changerJoueur:- joueurCourant(jaune), retract(joueurCourant(_)), assert(joueurCourant(rouge)).
changerJoueur:- joueurCourant(rouge), retract(joueurCourant(_)), assert(joueurCourant(jaune)).

membreMaxRank(X, [Y|L], R):- membreMax(L, 1, 1, R, Y, X),!.
membreMax([], _, Rm, Rm, Max, Max).
membreMax([Y|L], R1, Rm, R, Max, X):- incr(R1,R2), maximum(Y, Max, Rm, R2, NewMax, NewRankMax), membreMax(L,R2,NewRankMax,R, NewMax, X).

membreMinRank(X, [Y|L], R):- membreMin(L, 1, 1, R, Y, X),!.
membreMin([], _, Rm, Rm, Max, Max).		 
membreMin([Y|L], R1, Rm, R, Max, X):- incr(R1,R2), minimum(Y, Max, Rm, R2, NewMax, NewRankMax), membreMin(L,R2,NewRankMax,R, NewMax, X).

minimum(Y, Max, _, NewRankMax, Y, NewRankMax):- Y<Max.
minimum(_, Max,OldRankMax, _, Max, OldRankMax). 

maximum(Y, Max, _, NewRankMax, Y, NewRankMax):- Y>Max.
maximum(_, Max,OldRankMax, _, Max, OldRankMax). 



%%% Place un jeton

% placerJeton/3(-Colonne, +Ligne, -Couleur) 
% insère si possible un jeton dans la colonne donnée
% retourne la ligne d'insertion, ou no
placerJeton(X,Y,C) :- coupValide(X), insererJeton(X, Y, C).

%%%%% placerJeton %%%%%
% coupValide/1(-Colonne)
% Vérifie si un jeton est jouable dans cette colonne
% retourne yes ou no
coupValide(X) :- nbColonnes(NBCOLONNES), X=<NBCOLONNES, X>=1, nbLignes(NBLIGNES), caseVideTest(X,NBLIGNES).

% insererJeton/3(-Colonne, +Ligne, -Couleur)
% Insere, sans vérification, un jeton de la couleur donnée, dans la colonne donnée
% retourne la ligne d'insertion, 
insererJeton(X,Y,C) :- calculPositionJeton(X, 1, Y), assert(caseTest(X,Y,C)).

% calculPositionJeton/3(+Colonne,+LigneToCheck,-Ligne)
% calcule la premiere ligne vide d'une colonne
% retourne l'indice de cette ligne vide
calculPositionJeton(X,YCheck,YCheck) :- caseVideTest(X,YCheck), !.
calculPositionJeton(X,YCheck,Y) :- incr(YCheck, YCheck1), calculPositionJeton(X,YCheck1,Y).

caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).

%%%%% CODE DUPLIQUÉ EN ATTENDANT UNE MEILLEURE SOLUTION %%%%%

gagneTest(X,Y,J,V) :- %V=1 si victoire direct, 0 si indirect
	%assert(caseTest(X,Y,J)),
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
	testPotentiel(X,Y,J,P), %Peut on la remplir au prochain coup?
	testAccumulation(X,Y,J,A). %As-t-on accumulation?

testPotentiel(X,1,J,1).	%case au niveau 1
testPotentiel(X,Y,J,1):-
	decr(Y,Y1),
	caseTest(X,Y1,_).  %On peut la remplir
testPotentiel(X,Y,J,0). %On ne peut pas la remplir


testAccumulation(X,Y,J,1) :- gagneTestDirect(X,Y,J), sousTestAccumulation(X,Y,J). %La case est gagnante
testAccumulation(X,Y,J,0). %Pas d'accumulation.

sousTestAccumulation(X,Y,J) :- decr(Y,Y1), caseVideTest(X,Y1), gagneTestDirect(X,Y1,J). %Celle en dessous aussi
sousTestAccumulation(X,Y,J) :- incr(Y,Y1), caseVideTest(X,Y1), gagneTestDirect(X,Y1,J). %Celle au dessus aussi
	
	
testFinal(R,P,A,1):-
	R > 2.
testFinal(2,2,A,0):-
testFinal(R,P,A,-1):-
	A > 0.
	

gagneColonneTest(X,Y,J,1) :-
	decr(Y,Y1),
	caseTest(X,Y1,J),
	decr(Y1,Y2),
	caseTest(X,Y2,J),
	decr(Y2,Y3),
	caseTest(X,Y3,J). %ligne en bas

gagneLigneTest(X,Y,J,V) :-
	decr(X,X1),
	gaucheVerifTest(X1,Y,J,R1,P1,A1),
	incr(X,X2),
	droiteVerifTest(X2,Y,J,R2,P2,A2),
	!,
	Rfin is R1+R2, 
	Pfin is P1+P2, 
	Afin is A1+A2,
	testFinal(Rfin,Pfin,Afin,V).


gaucheVerifTest(X,Y,J,Rg,Pg,A):- %Rg cases r�elles accumul�es, Pg cases libres sur les c�t�s (2 max)
	gaucheTest(X,Y,J,0,Rg,Pg,A).
	
gaucheTest(X,Y,J,R,R,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A). %Faux seulement si case de l'autre couleur ou si coup dépasse du tableau
gaucheTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur � gauche ou fin du tableau
gaucheTest(X,Y,J,R,Rg,P,A) :-
	decr(X,X1),
	incr(R,R1),
	gaucheTest(X1,Y,J,R1,Rg,P,A).

droiteVerifTest(X,Y,J,Rg,Pg,A):-
	droiteTest(X,Y,J,0,Rg,Pg,A).
	
droiteTest(X,Y,J,R,R,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A). %Faux seulement si case de l'autre couleur ou si coup dépasse du tableau
droiteTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur � droite 
droiteTest(X,Y,J,R,Rg,P,A) :-
	incr(X,X1),
	incr(R,R1),
	droiteTest(X1,Y,J,R1,Rg,P,A).

gagneDiag1Test(X,Y,J,V) :-
	incr(Y,Y1),
	decr(X,X1),
	gaucheHautVerifTest(X1,Y1,J,R1,P1,A1),
	decr(Y,Y2),
	incr(X,X2),
	droiteBasVerifTest(X2,Y2,J,R2,P2,A2),
	!,
	Rfin is R1+R2, Pfin is P1+P2, Afin is A1+A2,
	testFinal(Rfin,Pfin,Afin,V).

gaucheHautVerifTest(X,Y,J,Rg,Pg,A):-
	gaucheHautTest(X,Y,J,0,Rg,Pg,A).
gaucheHautTest(X,Y,J,R,R,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A).  
gaucheHautTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur en bas � gauche
gaucheHautTest(X,Y,J,R,Rg,P,A) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHautTest(X1,Y1,J,R1,Rg,P,A).


droiteBasVerifTest(X,Y,J,Rg,Pg,A):-
	droiteBasTest(X,Y,J,0,Rg,Pg,A).
droiteBasTest(X,Y,J,R,R,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A). 
droiteBasTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur en bas � droite 
droiteBasTest(X,Y,J,R,Rg,P,A) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBasTest(X1,Y1,J,R1,Rg,P,A).

gagneDiag2Test(X,Y,J,V) :-
	decr(Y,Y1),
	decr(X,X1),
	gaucheBasVerifTest(X1,Y1,J,R1,P1,A1),
	incr(Y,Y2),
	incr(X,X2),
	droiteHautVerifTest(X2,Y2,J,R2,P2,A2),
	!,
	Rfin is R1+R2, Pfin is P1+P2, Afin is A1+A2,
	testFinal(Rfin,Pfin,Afin,V).

gaucheBasVerifTest(X,Y,J,Rg,Pg,A) :-
	gaucheBasTest(X,Y,J,0,Rg,Pg,A).
gaucheBasTest(X,Y,J,R,R,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A).
gaucheBasTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur en bas � gauche
gaucheBasTest(X,Y,J,R,Rg,P,A) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBasTest(X1,Y1,J,R1,Rg,P,A).


droiteHautVerifTest(X,Y,J,Rg,Pg,A) :-
	droiteHautTest(X,Y,J,0,Rg,Pg,A).
droiteHautTest(X,Y,J,R,R,P,A) :-
	testPotentielAccumulation(X,Y,J,P,A). 
droiteHautTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Case de l'autre couleur en haut � droite 
droiteHautTest(X,Y,J,R,Rg,P,A) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHautTest(X1,Y1,J,R1,Rg,P,A).

	
	
	
	
	
	
	
	
	
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