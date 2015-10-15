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

% runTest/3
% IA1 joue contre IA2 "NbIterations" fois le predicat affiche combien de fois qui a battu qui
runTest(NbIterations,IA1,IA2) :-
	NbIterationsParIA is NbIterations//2,
	runTestIAXFirst(NbIterationsParIA,IA1,IA2,0,NbFoisIA1GagneEnCommencant,0,NbFoisIA1PerdEnCommencant),
	runTestIAXFirst(NbIterationsParIA,IA2,IA1,0,NbFoisIA2GagneEnCommencant,0,NbFoisIA2PerdEnCommencant),
	write(NbFoisIA2GagneEnCommencant),write(NbFoisIA2PerdEnCommencant),
	nl,
	write(NbFoisIA1GagneEnCommencant),write(NbFoisIA1PerdEnCommencant).


%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

jeu(PartieNulle) :- 	
	tour(PartieNulle).

tour(PartieNulle) :- 
	joueurCourant(CouleurJCourant,TypeJoueur),
	aQuiDemanderCoup(CouleurJCourant,TypeJoueur,'',Coup),
	bouclePlacer(Coup,TypeJoueur,CouleurJCourant,Y),
	% evalJeu(CouleurJCourant,Score), write(Score),
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

init :- initJeu, retractall(joueurCourant(_,_)), retractall(autreJoueur(_,_)).


% test de sortie de runTestIAXFirst
runTestIAXFirst(0,_,_,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin) :-
	NbIA1GagneFin is NbIA1GagneIni,
	NbIA2GagneFin is NbIA2GagneIni,!.
runTestIAXFirst(NbIterations,IA1,IA2,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin) :-
	init,
	assert(joueurCourant(rouge,IA1)),
	assert(autreJoueur(jaune,IA2)),
	jeu(PartieNulle),
	joueurCourant(_,IAGagnante),
	incrementerGagnant(PartieNulle,IAGagnante,NbIA1GagneIni,NbIA1GagneFin1,NbIA2GagneIni,NbIA2GagneFin1,IA1,IA2),
	NbIterations2 is NbIterations-1,
	runTestIAXFirst(NbIterations2,IA1,IA2,NbIA1GagneFin1,NbIA1GagneFin,NbIA2GagneFin1,NbIA2GagneFin).

incrementerGagnant(true,_,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin,_,_) :-
	NbIA1GagneFin is NbIA1GagneIni,
	NbIA2GagneFin is NbIA2GagneIni.
incrementerGagnant(false,IAGagnante,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin,IA1,_) :-
	IA1==IAGagnante,
	NbIA1GagneFin is NbIA1GagneIni+1,
	NbIA2GagneFin is NbIA2GagneIni.
incrementerGagnant(false,IAGagnante,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin,_,IA2) :-
	IA2==IAGagnante,
	NbIA1GagneFin is NbIA1GagneIni,
	NbIA2GagneFin is NbIA2GagneIni+1.