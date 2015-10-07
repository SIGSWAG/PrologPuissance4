%%%%%%%%%%%%%%%%% Fichier jeu.pl %%%%%%%%%%%%%%%%%%%
nbLignes(5).
nbColonnes(8).


%%%%%%%%%%%%%%%%% Fonctions utiles %%%%%%%%%%%%%%%%%

incr(X,X1):- X1 is X+1.
decr(X,X1):- X1 is X-1.


%%%%%%%%%%%%%%%%% Initialisation du plateau %%%%%%%%%%%%%%%%%



init:- initClear, initLignes(0).

initClear :- retractall(case(X,Y,Z)).



initTest :- assert(case(1,1,rouge)), assert(case(1,2,rouge)), assert(case(1,3,rouge)), assert(case(1,4,rouge)). %initInterface, play

initLignes(Y) :- nbLignes(Y).
initLignes(Y):- initColonne(0,Y), incr(Y,Y1), initLignes(Y1).

initColonne(X,Y) :- nbColonnes(X).
initColonne(X,Y) :- assert(case(X,Y,vide)), incr(X,X1), initColonne(X1,Y).



%%%%%%%%%%%%%%%% Play %%%%%%%%%%%%%%%%%

play(X,Y,J).







gagneColonne(X,Y,J) :- case(X,Y,J), decr(Y,Y1), case(X,Y1,J), decr(Y1,Y2), case(X,Y2,J), decr(Y2,Y3), case(X,Y3,J). %ligne en bas

gagneLigne(X,Y,J) :- gauche(X,Y,J,Rg), droite(X,Y,J,Rd), (Rg+Rd)>3.



% coté, diagonaleS


% placerJeton\3(-Colonne, +Ligne, -Couleur) 
% insère si possible un jeton dans la colonne donnée
% retourne la ligne d'insertion, ou no
placerJeton(X,Y,C) :- coupValide(X), insererJeton(X, Y, C).

% coupValide\1(-Colonne)
% Vérifie si un jeton est jouable dans cette colonne
% retourne yes ou no
coupValide(X) :- nbLignes(NBLIGNES), not(case(X,NBLIGNES,Z)).

% insererJeton\3(-Colonne, +Ligne, -Couleur)
% Insere, sans vérification, un jeton de la couleur donnée, dans la colonne donnée
% retourne la ligne d'insertion, 
insererJeton(X,Y,C) :- calculPositionJeton(X, 0, Y), retract(case(X,Y,vide)), assert(case(X,Y,C)).

% calculPositionJeton\3(+Colonne,+LigneToCheck,-Ligne)
% calcule la premiere ligne vide d'une colonne
% retourne l'indice de cette ligne vide
calculPositionJeton(X,YCheck,YCheck) :- case(X,YCheck,vide), !.
calculPositionJeton(X,YCheck,Y) :- incr(YCheck, YCheck1), calculPositionJeton(X,YCheck1,Y).




