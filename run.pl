%%%%%%%%%%%%%%%%%%%%%%%%%%%% Run %%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Includes %%%%%%%%%%%%
:- [jeu].
:- [ihm].

%%%%%%%%%%%%%%%%%%%%%%%%%%% Constantes %%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%% Gestion du cycle de vie %%
run :- typeJeu(T), 
       init(T),
	   jeu,
	   afficherGagnant
	   .
	   
	   
	   
	   
jeu :- tour.



tour :- affiche(), bouclePlacer, not(gagne), changerJoueur, tour.


bouclePlacer :- placerJeton(Coup,Y,joueur).
bouclePlacer :- demandeCoup(joueur, Coup), bouclePlacer.