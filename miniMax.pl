%%%%%%%%%%%% miniMax.pl %%%%%%%%%%%%

:- use_module(util).

%%%%%%%%%%%%%%%%
%% Constantes %%
%%%%%%%%%%%%%%%%

profondeur(3).



%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

donneCoup(J,R) :- init(Pmax,J) minMax(J,Pmax,C), clearCaseTest.

minMax(J,P,C) :- changeJoueur(J), f(X,Y,J,0,P,R, []).



%P commence à 0, J à l'adversaire
f(X,Y,J,P,Pmax,R, L) :- gagneTest(X,Y,J), maximizer(J), R is 1000.
f(X,Y,J,P,Pmax,R, L) :- gagneTest(X,Y,J), not(maximizer(J)), R is -1.
f(X,Y,J,P,Pmax,R, L) :- P == Pmax, evalJeu(J,R).
f(X,Y,J,P,Pmax,R, L) :- incr(P,P1), changeJoueur(J1), placerJeton(X,Y1,J1), callf(Y1,J1,P1,Pmax,R,[], L), max_member(R,L1).

callf(Y1,J1,P1,Pmax,R,L) :- L is [1|L] f(X,Y1,J1,P1,Pmax,R1, L1), L is [2|L], f(X,Y1,J1,P1,Pmax,R2, L1),f(X,Y1,J1,P1,Pmax,R3, L1),f(X,Y1,J1,P1,Pmax,R4, L1),f(X,Y1,J1,P1,Pmax,R5, L1),f(X,Y1,J1,P1,Pmax,R6, L1),f(X,Y1,J1,P1,Pmax,R7, L1),f(X,Y1,J1,P1,Pmax,R8, L1), max_member(R, [R1,R2,R3,R4,R5,R6,R7,R8]).

addToList(X, L, [X|L]). 









%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

caseTest(X,Y,J) :- case(X,Y,J).

init(Pmax,J) :- profondeur(Pmax), assert(maximizer(J)), assert(joueurCourant(J)),.

clearCaseTest :- retractall(caseTest(_,_,_)), retract(maximizer(X)), retract(joueurCourant(X)).

changeJoueur(rouge):-joueurCourant(jaune), retract(joueurCourant(jaune)), assert(joueurCourant(rouge)).
changeJoueur(jaune):-joueurCourant(rouge), retract(joueurCourant(rouge)), assert(joueurCourant(jaune)).

%%%%% gagneTest %%%%%

% gagneTest/3(+colonne, +ligne, +joueur)
% vérifie si le coup est gagnant pour joueur
% retourne yes si gagnant ou no sinon
gagneTest(X,Y,J) :- gagneTestColonne(X,Y,J).
gagneTest(X,Y,J) :- gagneTestLigne(X,Y,J).
gagneTest(X,Y,J) :- gagneTestDiag1(X,Y,J).
gagneTest(X,Y,J) :- gagneTestDiag2(X,Y,J). 

%%% En colonne %%%

gagneTestColonne(X,Y,J) :- caseTest(X,Y,J), decr(Y,Y1), caseTest(X,Y1,J), decr(Y1,Y2), caseTest(X,Y2,J), decr(Y2,Y3), caseTest(X,Y3,J). %ligne en bas

%%% En ligne %%%

gagneTestLigne(X,Y,J) :- gaucheVerif(X,Y,J,Rg), droiteVerif(X,Y,J,Rd),!, Rf is Rg+Rd, Rf>4.

gaucheVerif(X,Y,J,Rg):- gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :- not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
gauche(X,Y,J,R,Rg) :- decr(X,X1), incr(R,R1), gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):- droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :- not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
droite(X,Y,J,R,Rg) :- incr(X,X1), incr(R,R1), droite(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneTestDiag1(X,Y,J) :- gaucheHautVerif(X,Y,J,Rg), droiteBasVerif(X,Y,J,Rd),!, Rf is Rg+Rd, Rf>4.

gaucheHautVerif(X,Y,J,Rg):- gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :- not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
gaucheHaut(X,Y,J,R,Rg) :- incr(Y,Y1), decr(X,X1), incr(R,R1), gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):- droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :- not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
droiteBas(X,Y,J,R,Rg) :- decr(Y,Y1), incr(X,X1), incr(R,R1), droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneTestDiag2(X,Y,J) :- gaucheBasVerif(X,Y,J,Rg), droiteHautVerif(X,Y,J,Rd),!, Rf is Rg+Rd, Rf>4.

gaucheBasVerif(X,Y,J,Rg):- gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :- not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
gaucheBas(X,Y,J,R,Rg) :- decr(Y,Y1), decr(X,X1), incr(R,R1), gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg):- droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :- not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
droiteHaut(X,Y,J,R,Rg) :- incr(Y,Y1), incr(X,X1), incr(R,R1), droiteHaut(X1,Y1,J,R1,Rg).



%%%%% placerJeton %%%%%

%%% Place un jeton

% placerJeton/3(-Colonne, +Ligne, -Couleur) 
% insère si possible un jeton dans la colonne donnée
% retourne la ligne d'insertion, ou no
placerJeton(X,Y,C) :- coupValide(X), insererJeton(X, Y, C).

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

coupPossible :- nbColonnes(NBCOLLONNES), between(1,NBCOLLONNES,X), coupValide(X).