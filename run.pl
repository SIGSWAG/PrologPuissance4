%%%%%%%%%%%%%%%%%%%%%%%%%%%% Run %%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Includes %%%%%%%%%%%%
:- [jeu].
:- [ihm].

%%%%%%%%%%%%%%%%%%%%%%%%%%% Constantes %%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%% Gestion du cycle de vie %%
run 	:-
		demandeTypeDeJeu(TypeJoueur1),
		demandeTypeDeJeu(TypeJoueur2),
     	init,
     	random_select(TypeJoueurR,[TypeJoueur1,TypeJoueur2],[TypeJoueurJ|_]),
	   	assert(joueurCourant(rouge,TypeJoueurR)),
	   	assert(autreJoueur(jaune,TypeJoueurJ)),
	   	jeu,
		joueurCourant(CouleurGagnante,TypeJoueurGagnant),
		autreJoueur(CouleurPerdante,TypeJoueurPerdant),
		getTypeJoueurString(TypeJoueurGagnant,TypeJoueurGagnantString),
		getTypeJoueurString(TypeJoueurPerdant,TypeJoueurPerdantString),
	   	afficherGagnant(CouleurGagnante,CouleurPerdante,TypeJoueurGagnantString,TypeJoueurPerdantString).
	   
jeu 	:- 	
		tour.

tour 	:- 
		afficher,
		joueurCourant(CouleurJCourant,TypeJoueur),
		aQuiDemanderCoup(CouleurJCourant,TypeJoueur,'',Coup),
		bouclePlacer(Coup,TypeJoueur,CouleurJCourant,Y),
		testVictoire(Coup,Y,CouleurJCourant).

bouclePlacer(Coup,_,CouleurJCourant,Y) :-
	placerJeton(Coup,Y,CouleurJCourant),!.
bouclePlacer(_,TypeJoueur,CouleurJCourant,Y) :-
	aQuiDemanderCoup(CouleurJCourant,TypeJoueur,'Votre coup n\'est pas valide. Veuillez reessayer.\n',Coup),
	bouclePlacer(Coup,TypeJoueur,CouleurJCourant,Y).

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

testVictoire(Coup,Y,CouleurJCourant) :- gagne(Coup,Y,CouleurJCourant), afficher.
testVictoire(_,_,_) :- changerJoueur, tour.

% permet d'appeler l'ihm ou les IAs pour récupérer le coup suivant
% 1==humain
aQuiDemanderCoup(CouleurJCourant,1,Message,Coup) :- demandeCoup(CouleurJCourant,Message,Coup),!.
% 2==IA aleatoire
aQuiDemanderCoup(CouleurJCourant,2,Message,Coup) :- write('rouge?'),demandeCoup(CouleurJCourant,Message,Coup).
% etc ...

getTypeJoueurString(1,TypeJoueurString) :- TypeJoueurString='Humain',!.
getTypeJoueurString(2,TypeJoueurString) :- TypeJoueurString='IA Aleatoire'.
