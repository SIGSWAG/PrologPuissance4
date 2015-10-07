%%%%%%%%%%%%%%%%% Fichier jeu.pl %%%%%%%%%%%%%%%%%%%

% Initialisation du plateau

incr(X,X1):- X1 is X+1.


init:- retractall(case(X,Y,Z)), initLignes(0). %initInterface, play

initLignes(5).
initLignes(Y):- initColonne(0,Y), incr(Y,Y1), initLignes(Y1).

initColonne(4,Y).
initColonne(X,Y) :- assert(case(X,Y,vide)), incr(X,X1), initColonne(X1,Y).


% Play

play (X,Y,J) :- 







aGagne (X,Y,J).



gagne(X, X+1, X+2, X+3, Y).