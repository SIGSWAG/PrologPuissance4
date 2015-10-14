%%%%%%%%%%%% run.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(ia).
:- use_module(ihm).
:- use_module(eval).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

run :-
	init,
	demandeTypeDeJeu(TypeJoueur1),
	demandeTypeDeJeu(TypeJoueur2),
	%random_select(TypeJoueurR,[TypeJoueur1,TypeJoueur2],[TypeJoueurJ|_]),
	TypeJoueurR=TypeJoueur1,
	TypeJoueurJ=TypeJoueur2,
	assert(joueurCourant(rouge,TypeJoueurR)),
	assert(autreJoueur(jaune,TypeJoueurJ)),
	jeu(PartieNulle),
	afficherFin(PartieNulle).


%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

jeu(PartieNulle) :- 	
	tour(PartieNulle).

tour(PartieNulle) :- 
	joueurCourant(CouleurJCourant,TypeJoueur),
	aQuiDemanderCoup(CouleurJCourant,TypeJoueur,'',Coup),
	bouclePlacer(Coup,TypeJoueur,CouleurJCourant,Y),
	evalJeu(CouleurJCourant,Score), write(Score),
	testFin(Coup,Y,CouleurJCourant, PartieNulle).

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

testFin(Coup,Y,CouleurJCourant,PartieNulle) :- gagne(Coup,Y,CouleurJCourant), PartieNulle=false, afficher.
testFin(_,_,_,PartieNulle) :- not(coupPossible), PartieNulle=true, afficher.
testFin(_,_,_,PartieNulle) :- changerJoueur, tour(PartieNulle).

% permet d'appeler l'ihm ou les IAs pour récupérer le coup suivant
% 1==humain
aQuiDemanderCoup(CouleurJCourant,1,Message,Coup) :- afficher, demandeCoup(CouleurJCourant,Message,Coup),!.
% 2==IA aleatoire
aQuiDemanderCoup(_,2,_,Coup) :- iaAleatoire(Coup).
% etc ...

getTypeJoueurString(1,'Humain').
getTypeJoueurString(2,'IA Aléatoire').

% partie non nulle
afficherFin(false) :-
	joueurCourant(CouleurGagnante,TypeJoueurGagnant),
	autreJoueur(CouleurPerdante,TypeJoueurPerdant),
	getTypeJoueurString(TypeJoueurGagnant,TypeJoueurGagnantString),
	getTypeJoueurString(TypeJoueurPerdant,TypeJoueurPerdantString),
   	afficherGagnant(CouleurGagnante,CouleurPerdante,TypeJoueurGagnantString,TypeJoueurPerdantString).
% partie nulle
afficherFin(true) :-
	afficherPartieNulle.