%%%%%%%%%%%%%%%%% Fichier jeu.pl %%%%%%%%%%%%%%%%%%%


% Initialisation du plateau

incr(X,X1):- X1 is X+1.
decr(X,X1):- X1 is X-1.


init:- initClear, initConstantes, initLignes(0), initTest.

initClear :- retractall(case(X,Y,Z)), retract(nbLignes(C)), retract(nbColonnes(C)).

initConstantes :- assert(nbLignes(5)), assert(nbColonnes(4)).

initTest :- assert(case(1,1,rouge)), assert(case(1,2,rouge)), assert(case(1,3,rouge)), assert(case(1,4,rouge)). %initInterface, play

initLignes(Y) :- nbLignes(Y).
initLignes(Y):- initColonne(0,Y), incr(Y,Y1), initLignes(Y1).

initColonne(X,Y) :- nbColonnes(X).
initColonne(X,Y) :- assert(case(X,Y,vide)), incr(X,X1), initColonne(X1,Y).


% Play

play(X,Y,J).







gagneColonne(X,Y,J) :- case(X,Y,J), decr(Y,Y1), case(X,Y1,J), decr(Y1,Y2), case(X,Y2,J), decr(Y2,Y3), case(X,Y3,J). %ligne en bas

gagneLigne(X,Y,J,[]).
gagneLigne(X,Y,J,L) .
gagneLigne(X,Y,J,L) . 
% coté, diagonaleS



