%%%%%%%%%%%%%%%%% Fichier jeu.pl %%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%% Constantes %%%%%%%%%%%%%%%%%

nbLignes(5).
nbColonnes(8).

%%%%%%%%%%%%%%%%% Fonctions utiles %%%%%%%%%%%%%%%%%

incr(X,X1):- X1 is X+1.
decr(X,X1):- X1 is X-1.


%%%%%%%%%%%%%%%%% Initialisation du plateau %%%%%%%%%%%%%%%%%



init:- initClear.

initClear :- retractall(case(X,Y,Z)).

initTest :- assert(case(1,1,rouge)), assert(case(0,1,rouge)), assert(case(2,1,rouge)), assert(case(3,1,bleu)), assert(case(4,1,rouge)). %initInterface, play



%%%%%%%%%%%%%%%% Play %%%%%%%%%%%%%%%%%

play(X,Y,J).






gagneColonne(X,Y,J) :- case(X,Y,J), decr(Y,Y1), case(X,Y1,J), decr(Y1,Y2), case(X,Y2,J), decr(Y2,Y3), case(X,Y3,J). %ligne en bas


gagneLigne(X,Y,J) :- gaucheVerif(X,Y,J,Rg), droiteVerif(X,Y,J,Rd), (Rg+Rd)>4.


gaucheVerif(X,Y,J,Rg):- gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'à la case non J
gauche(X,Y,J,R,Rg) :- decr(X,X1), incr(R,R1), gauche(X1,Y,J,R1,Rg).


droiteVerif(X,Y,J,Rg):- droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'à la case non J
droite(X,Y,J,R,Rg) :- incr(X,X1), incr(R,R1), droite(X1,Y,J,R1,Rg).

% coté, diagonaleS



