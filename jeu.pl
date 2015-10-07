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

initTest :- assert(case(4,1,rouge)), assert(case(3,2,rouge)), assert(case(2,3,rouge)), assert(case(1,4,rouge)). %initInterface, play



%%%%%%%%%%%%%%%% Play %%%%%%%%%%%%%%%%%

play(X,Y,J).


%%%%%%%%%%%%%%%% Vérification de la victoire %%%%%%%%%%%%%

gagne(X,Y,J) :- gagneColonne(X,Y,J).
gagne(X,Y,J) :- gagneLigne(X,Y,J).
gagne(X,Y,J) :- gagneDiag1(X,Y,J).
gagne(X,Y,J) :- gagneDiag2(X,Y,J).

%%% En colonne %%%

gagneColonne(X,Y,J) :- case(X,Y,J), decr(Y,Y1), case(X,Y1,J), decr(Y1,Y2), case(X,Y2,J), decr(Y2,Y3), case(X,Y3,J). %ligne en bas

%%% En ligne %%%

gagneLigne(X,Y,J) :- gaucheVerif(X,Y,J,Rg), droiteVerif(X,Y,J,Rd),!, Rf is Rg+Rd, Rf>4.

gaucheVerif(X,Y,J,Rg):- gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'à la case non J
gauche(X,Y,J,R,Rg) :- decr(X,X1), incr(R,R1), gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):- droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'à la case non J
droite(X,Y,J,R,Rg) :- incr(X,X1), incr(R,R1), droite(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneDiag1(X,Y,J) :- gaucheHautVerif(X,Y,J,Rg), droiteBasVerif(X,Y,J,Rd),!, Rf is Rg+Rd, Rf>4.

gaucheHautVerif(X,Y,J,Rg):- gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'à la case non J
gaucheHaut(X,Y,J,R,Rg) :- incr(Y,Y1), decr(X,X1), incr(R,R1), gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):- droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'à la case non J
droiteBas(X,Y,J,R,Rg) :- decr(Y,Y1), incr(X,X1), incr(R,R1), droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneDiag2(X,Y,J) :- gaucheBasVerif(X,Y,J,Rg), droiteHautVerif(X,Y,J,Rd),!, Rf is Rg+Rd, Rf>4.

gaucheBasVerif(X,Y,J,Rg):- gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'à la case non J
gaucheBas(X,Y,J,R,Rg) :- decr(Y,Y1), decr(X,X1), incr(R,R1), gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg):- droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'à la case non J
droiteHaut(X,Y,J,R,Rg) :- incr(Y,Y1), incr(X,X1), incr(R,R1), droiteHaut(X1,Y1,J,R1,Rg).

