%%%%%%%%%%%%%%%%%%%%%%%%%%%% Run %%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Includes %%%%%%%%%%%%
:- [jeu].
:- [ihm].

%%%%%%%%%%%%%%%%%%%%%%%%%%% Constantes %%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%% Gestion du cycle de vie %%
run 	:- 	demandeTypeDeJeu(_), 
     	init, % à améliorer pour initialiser selon le type de jeu
	   	assert(joueurCourant(rouge)),
	   	jeu,
		joueurCourant(Joueur),
	   	afficherGagnant(Joueur).
	   
jeu 	:- 	tour.

tour :- 	afficher,
		joueurCourant(Joueur),
		demandeCoup(Joueur,Coup),
		bouclePlacer(Coup,Joueur,Y),
		not(gagne(Coup,Y,Joueur)),
		changerJoueur,
		tour.

bouclePlacer(Coup,Joueur,Y) :- placerJeton(Coup,Y,Joueur).
bouclePlacer(_,Joueur,Y) :- demandeCoup(Joueur,Coup), bouclePlacer(Coup,Joueur,Y).

changerJoueur :- joueurCourant(rouge), retractall(joueurCourant(_)), assert(joueurCourant(jaune)), !.
changerJoueur :- joueurCourant(jaune), retractall(joueurCourant(_)), assert(joueurCourant(rouge)).