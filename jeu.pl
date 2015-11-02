%%%%%%%%%%%% jeu.pl %%%%%%%%%%%%

:- module(jeu, [
	initJeu/0,
	gagne/3,
	placerJeton/3,
	coupPossible/0,
	case/3,
	coupValide/1,
	typeJoueur/2,
	changerJoueur/0
]).

:- use_module(util).

:- dynamic case/3.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

%%% Gestion des joueurs %%%

typeJoueur(1,'Humain').
typeJoueur(2,'IA Aleatoire').
typeJoueur(3,'IA Minimax 3').
typeJoueur(4,'IA Minimax 4').
typeJoueur(5,'IA Minimax 5').

changerJoueur :-
	joueurCourant(rouge,TypeJoueurR), 
	autreJoueur(jaune,TypeJoueurJ),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)),
	assert(joueurCourant(jaune,TypeJoueurJ)),
	assert(autreJoueur(rouge,TypeJoueurR)),!.
changerJoueur :-
	joueurCourant(jaune,TypeJoueurJ),
	autreJoueur(rouge,TypeJoueurR),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)),
	assert(joueurCourant(rouge,TypeJoueurR)),
	assert(autreJoueur(jaune,TypeJoueurJ)),!.


%%% Prédicats utiles

%%% Initialisation du plateau

% initJeu/0
% vide le plateau, initialise un nouveau plateau vide
% retourne yes
initJeu :-
	initClear.

% coupPossible/0
% verifie si l on peut encore joueur
% vrai si il reste des coups valides, faux sinon
coupPossible :-
	nbColonnes(NBCOLLONNES),
	between(1,NBCOLLONNES,X),
	coupValide(X).

%%% Vérification de la victoire 

% gagne/3(+colonne, +ligne, +joueur)
% vérifie si le coup est gagnant pour joueur
% retourne yes si gagnant ou no sinon
gagne(X,Y,J) :-
	gagneColonne(X,Y,J).
gagne(X,Y,J) :-
	gagneLigne(X,Y,J).
gagne(X,Y,J) :-
	gagneDiag1(X,Y,J).
gagne(X,Y,J) :-
	gagneDiag2(X,Y,J).


%%% Place un jeton

% placerJeton/3(-Colonne, +Ligne, -Couleur) 
% insère si possible un jeton dans la colonne donnée
% retourne la ligne d'insertion, ou no
placerJeton(X,Y,C) :-
	coupValide(X),
	insererJeton(X, Y, C).

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%


%%%%% init %%%%%


initClear :-
	retractall(case(_,_,_)). % pourrait fonctionner avec :- dynamic, à investiguer

initTest :-
	assert(case(4,1,rouge)),
	assert(case(3,2,rouge)),
	assert(case(2,3,rouge)),
	assert(case(1,4,rouge)). %initInterface, play


%%%%% gagne %%%%%


%%% En colonne %%%

gagneColonne(X,Y,J) :-
	case(X,Y,J),
	decr(Y,Y1),
	case(X,Y1,J),
	decr(Y1,Y2),
	case(X,Y2,J),
	decr(Y2,Y3),
	case(X,Y3,J). %ligne en bas

%%% En ligne %%%

gagneLigne(X,Y,J) :-
	gaucheVerif(X,Y,J,Rg),
	droiteVerif(X,Y,J,Rd),
	!,
	Rf is Rg+Rd, Rf>4.

gaucheVerif(X,Y,J,Rg):-
	gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'à la case non J
gauche(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):-
	droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'à la case non J
droite(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droite(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneDiag1(X,Y,J) :-
	gaucheHautVerif(X,Y,J,Rg),
	droiteBasVerif(X,Y,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>4.

gaucheHautVerif(X,Y,J,Rg):-
	gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'à la case non J
gaucheHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):-
	droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'à la case non J
droiteBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneDiag2(X,Y,J) :-
	gaucheBasVerif(X,Y,J,Rg),
	droiteHautVerif(X,Y,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>4.

gaucheBasVerif(X,Y,J,Rg) :-
	gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'à la case non J
gaucheBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg) :-
	droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'à la case non J
droiteHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHaut(X1,Y1,J,R1,Rg).


%%%%% placerJeton %%%%%


% coupValide/1(-Colonne)
% Vérifie si un jeton est jouable dans cette colonne
% retourne yes ou no
coupValide(X) :-
	nbColonnes(NBCOLONNES),
	X=<NBCOLONNES,
	X>=1,
	nbLignes(NBLIGNES),
	caseVide(X,NBLIGNES).

% insererJeton/3(-Colonne, +Ligne, -Couleur)
% Insere, sans vérification, un jeton de la couleur donnée, dans la colonne donnée
% retourne la ligne d'insertion, 
insererJeton(X,Y,C) :-
	calculPositionJeton(X, 1, Y),
	assert(case(X,Y,C)).

% calculPositionJeton/3(+Colonne,+LigneToCheck,-Ligne)
% calcule la premiere ligne vide d'une colonne
% retourne l'indice de cette ligne vide
calculPositionJeton(X,YCheck,YCheck) :-
	caseVide(X,YCheck),
	!.
calculPositionJeton(X,YCheck,Y) :-
	incr(YCheck, YCheck1),
	calculPositionJeton(X,YCheck1,Y).

