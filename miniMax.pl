%%%%%%%%%%%% miniMax.pl %%%%%%%%%%%%

:- module(minimax, [parcoursArbre/4, caseTest/3]).

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

initCaseTest:- case(X,Y,Z), assert(caseTest(X,Y,Z)),false. %on assert une caseTest pour toutes les cases.
initCaseTest.

clearTest:- retractall(caseTest(X,Y,Z)), retractall(feuille(X,Y)), retract(maximizer(X)), retract(joueurCourant(J)). % on eve tout ce que l'on a ajouté.

%Parcours
%+X la colonne a jouer
%+P la profondeur courante
%+L la liste de coups courante
%+Beta,+Alpha les valeurs courantes pour Alpha et Beta.

parcours(X, P, Pmax, L, Beta, Alpha):- nbLignes(MaxLignes),case(X,MaxLignes,_), joueurCourant(Joue), maximizer(Joue), infiniteNeg(Value), assert(feuille(L, Value)). % on ne peut PAS jouer, on met -infini
parcours(X, P, Pmax, L, Beta, Alpha):- nbLignes(MaxLignes),case(X,MaxLignes,_), joueurCourant(Joue), not(maximizer(Joue)), infinitePos(Value), assert(feuille(L, Value)). % on ne peut PAS jouer, on met -infini
parcours(X, P, Pmax, L, Beta, Alpha):- nbLignes(MaxLignes),caseTest(X,MaxLignes,_), joueurCourant(Joue), evaluate(X,MaxLignes,Joue,Value), assert(feuille(L, Value)) .% on ne peut plus jouer, on met une feuille (on évalue)

parcours(X, P, Pmax, L, Beta, Alpha):-  joueurCourant(Joue), calculPositionJeton(X, 1, Y), gagneTest(X,Y,Joue), maximizer(Joue), infinitePos(Value), assert(feuille(L, Value)),retract(caseTest(X,Y,Joue)). %Victoire du max
parcours(X, P, Pmax, L, Beta, Alpha):-  joueurCourant(Joue), calculPositionJeton(X, 1, Y), gagneTest(X,Y,Joue), not(maximizer(Joue)), infiniteNeg(Value), assert(feuille(L, Value)),retract(caseTest(X,Y,Joue)). %Victoire du min

parcours(X, P, Pmax, L, Beta, Alpha):- P==Pmax,joueurCourant(Joue), placerJeton(X,Y,Joue), evaluate(X, Y, Joue, Value),assert(feuille(L, Value)),retract(caseTest(X,Y,Joue)). % on est à la prof max, on evalue et on met une feuille
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

	joueCoupSuivant(Val6,7,P1,Pmax,L,Beta6,Alpha6,Valeur,Beta7,Alpha7),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	retract(caseTest(X,Y,Joue)), %on annule le coup pour poursuivre dans l'arbre
	feuille([1|L], X1),feuille([2|L], X2), %on cherche les feuilles associées (elles ont été calculées plus bas dans l'arbre)
	setJoueur(P1), %on change de joueur
	assert(feuille(L,Valeur)),joueurCourant(Joueur). %on met notre feuille calculée

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
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L,Beta,Alpha,Val,Beta,Alpha):-joueurCourant(Joue), not(maximizer(Joue)), ValeurPrec =< Alpha, Val is ValeurPrec, assert(feuille([ColonneAJouer|L], Val)).%coupure alpha !!
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L,Beta,Alpha,Val,BetaCalc,Alpha):-joueurCourant(Joue), not(maximizer(Joue)), BetaCalc is min(Beta, ValeurPrec), parcours(ColonneAJouer, P1,Pmax,[ColonneAJouer|L],BetaCalc, Alpha), feuille([ColonneAJouer|L], ValeurFils), Val is min(ValeurFils, ValeurPrec). %pas de coupure!


%%For the Maximizer
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L, Beta, Alpha,Val,Beta,Alpha):-joueurCourant(Joue), maximizer(Joue), ValeurPrec >= Beta, Val is ValeurPrec,assert(feuille([ColonneAJouer|L], Val)).%coupure beta !!
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L, Beta, Alpha,Val,Beta,AlphaCalc):-joueurCourant(Joue), maximizer(Joue), AlphaCalc is max(Alpha, ValeurPrec), parcours(ColonneAJouer, P1,Pmax,[ColonneAJouer|L],Beta, AlphaCalc),feuille([ColonneAJouer|L], ValeurFils),Val is max(ValeurFils, ValeurPrec). %pas de coupure!





%%%%%%%%%% Aide calcul. Redundant code, do not remove %%%%%%%%%%%%%
setJoueur(P):- parite(P), maximizer(jaune), joueurCourant(jaune),retract(joueurCourant(X)), assert(joueurCourant(rouge)),!. % si P pair, alors c'est au minimizer de jouer
setJoueur(P):- parite(P), maximizer(rouge), joueurCourant(rouge),retract(joueurCourant(X)), assert(joueurCourant(jaune)),!.
setJoueur(P):- not(parite(P)),maximizer(rouge), joueurCourant(jaune),retract(joueurCourant(X)), assert(joueurCourant(rouge)),!. % P impair, maximizer joue
setJoueur(P):- not(parite(P)),maximizer(jaune), joueurCourant(rouge),retract(joueurCourant(X)), assert(joueurCourant(jaune)).
setJoueur(P).

choixValeurNoeud(L,R,Value):- joueurCourant(X), maximizer(X), coupAJouerMaximizer(L,R,Value),!. %on choisit val min ou max
choixValeurNoeud(L,R,Value):- coupAJouerMinimizer(L,R,Value).


coupAJouerMaximizer(L, R,X):- membreMaxRank(X,L,R).
coupAJouerMinimizer(L, R,X):- membreMinRank(X,L,R).


parite(X):- divmod(X ,2, Q, 0).

changerJoueur:- joueurCourant(jaune), retract(joueurCourant(X)), assert(joueurCourant(rouge)).
changerJoueur:- joueurCourant(rouge), retract(joueurCourant(X)), assert(joueurCourant(jaune)).

membreMaxRank(X, [Y|L], R):- membreMax(L, 1, 1, R, Y, X),!.
membreMax([], R, Rm, Rm, Max, Max).		 
membreMax([Y|L], R1, Rm, R, Max, X):- incr(R1,R2), maximum(Y, Max, Rm, R2, NewMax, NewRankMax), membreMax(L,R2,NewRankMax,R, NewMax, X).

membreMinRank(X, [Y|L], R):- membreMin(L, 1, 1, R, Y, X),!.
membreMin([], R, Rm, Rm, Max, Max).		 
membreMin([Y|L], R1, Rm, R, Max, X):- incr(R1,R2), minimum(Y, Max, Rm, R2, NewMax, NewRankMax), membreMin(L,R2,NewRankMax,R, NewMax, X).

minimum(Y, Max, OldRankMax, NewRankMax, Y, NewRankMax):- Y<Max.
minimum(Y, Max,OldRankMax, NewRankMax, Max, OldRankMax). 

maximum(Y, Max, OldRankMax, NewRankMax, Y, NewRankMax):- Y>Max.
maximum(Y, Max,OldRankMax, NewRankMax, Max, OldRankMax). 



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

gagneTest(X,Y,J) :-
	gagneColonneTest(X,Y,J).
gagneTest(X,Y,J) :-
	gagneLigneTest(X,Y,J).
gagneTest(X,Y,J) :-
	gagneDiag1Test(X,Y,J).
gagneTest(X,Y,J) :-
	gagneDiag2Test(X,Y,J).

gagneColonneTest(X,Y,J) :-
	decr(Y,Y1),
	caseTest(X,Y1,J),
	decr(Y1,Y2),
	caseTest(X,Y2,J),
	decr(Y2,Y3),
	caseTest(X,Y3,J). %ligne en bas

gagneLigneTest(X,Y,J) :-
	decr(X,X1),
	gaucheVerifTest(X1,Y,J,Rg),
	incr(X,X2),
	droiteVerifTest(X2,Y,J,Rd),
	!,
	Rf is Rg+Rd, Rf>2.

gaucheVerifTest(X,Y,J,Rg):-
	gaucheTest(X,Y,J,0,Rg).
gaucheTest(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheTest(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gaucheTest(X1,Y,J,R1,Rg).

droiteVerifTest(X,Y,J,Rg):-
	droiteTest(X,Y,J,0,Rg).
droiteTest(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteTest(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droiteTest(X1,Y,J,R1,Rg).

gagneDiag1Test(X,Y,J) :-
	incr(Y,Y1),
	decr(X,X1),
	gaucheHautVerifTest(X1,Y1,J,Rg),
	decr(Y,Y2),
	incr(X,X2),
	droiteBasVerifTest(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheHautVerifTest(X,Y,J,Rg):-
	gaucheHautTest(X,Y,J,0,Rg).
gaucheHautTest(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheHautTest(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHautTest(X1,Y1,J,R1,Rg).

droiteBasVerifTest(X,Y,J,Rg):-
	droiteBasTest(X,Y,J,0,Rg).
droiteBasTest(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteBasTest(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBasTest(X1,Y1,J,R1,Rg).

gagneDiag2Test(X,Y,J) :-
	decr(Y,Y1),
	decr(X,X1),
	gaucheBasVerifTest(X1,Y1,J,Rg),
	incr(Y,Y2),
	incr(X,X2),
	droiteHautVerifTest(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheBasVerifTest(X,Y,J,Rg) :-
	gaucheBasTest(X,Y,J,0,Rg).
gaucheBasTest(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheBasTest(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBasTest(X1,Y1,J,R1,Rg).

droiteHautVerifTest(X,Y,J,Rg) :-
	droiteHautTest(X,Y,J,0,Rg).
droiteHautTest(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteHautTest(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHautTest(X1,Y1,J,R1,Rg).