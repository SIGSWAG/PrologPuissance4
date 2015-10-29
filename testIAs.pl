%%%%%%%%%%%% run.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(ia).
:- use_module(eval).
:- use_module(miniMax).

:- dynamic joueurCourant/2.
:- dynamic autreJoueur/2.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%
% runTest/3
% NbIterations: le nombre de parties à jouer,
% IA1 et IA2 : les identifiants des 2 IA à confronter
% IA1 joue contre IA2 "NbIterations" fois le predicat affiche combien de fois qui a battu qui
runTest(NbIterations,IA1,IA2) :-
	NbIterationsParIA is NbIterations//2,
	runTestIAXEnPremier(NbIterationsParIA,IA1,IA2,0,NbFoisIA1GagneEnCommencant,0,NbFoisIA1PerdEnCommencant),
	runTestIAXEnPremier(NbIterationsParIA,IA2,IA1,0,NbFoisIA2GagneEnCommencant,0,NbFoisIA2PerdEnCommencant),
	typeJoueur(IA1,TypeIA1),
	typeJoueur(IA2,TypeIA2),
	write(TypeIA2), write(' en commençant : a gagné '), write(NbFoisIA2GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA2PerdEnCommencant),write(' fois.'),
	nl,
	write(TypeIA1), write(' en commençant : a gagné '), write(NbFoisIA1GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA1PerdEnCommencant),write(' fois.'),
	!.

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

% test de sortie de runTestIAXEnPremier
runTestIAXEnPremier(0,_,_,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneIni) :- !.
runTestIAXEnPremier(NbIterations,IA1,IA2,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin) :-
	init,
	assert(joueurCourant(rouge,IA1)), % rouge = IA1
	assert(autreJoueur(jaune,IA2)),
	jeu(PartieNulle),
	joueurCourant(CouleurIAGagnante,_),
	incrementerGagnant(PartieNulle,CouleurIAGagnante,NbIA1GagneIni,NbIA1GagneFin1,NbIA2GagneIni,NbIA2GagneFin1,IA1,IA2),
	NbIterations2 is NbIterations-1,
	runTestIAXEnPremier(NbIterations2,IA1,IA2,NbIA1GagneFin1,NbIA1GagneFin,NbIA2GagneFin1,NbIA2GagneFin).

incrementerGagnant(true,_,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneIni,_,_).
incrementerGagnant(false,rouge,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneIni,IA1,_) :-
	NbIA1GagneFin is NbIA1GagneIni+1.
incrementerGagnant(false,jaune,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneFin,_,IA2) :-
	NbIA2GagneFin is NbIA2GagneIni+1.

jeu(PartieNulle) :- 	
	tour(PartieNulle).

tour(PartieNulle) :- 
	joueurCourant(CouleurJCourant,TypeJoueur),
	obtenirCoup(CouleurJCourant,TypeJoueur,Coup),
	placerJeton(Coup,Y,CouleurJCourant),
	testFin(Coup,Y,CouleurJCourant, PartieNulle).

testFin(Coup,Y,CouleurJCourant,PartieNulle) :-
	gagne(Coup,Y,CouleurJCourant),
	PartieNulle=false.
testFin(_,_,_,PartieNulle) :-
	not(coupPossible),
	PartieNulle=true.
testFin(_,_,_,PartieNulle) :-
	changerJoueur,
	tour(PartieNulle).

init :-
	initJeu,
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)).

% permet d'appeler l'ihm ou les IAs pour récupérer le coup suivant
% 2==IA aleatoire
obtenirCoup(_,2,Coup) :-
    iaAleatoire(Coup),!.
% 3==minimax
obtenirCoup(JoueurCourant,3,Coup) :-
    iaMinimax(JoueurCourant,Coup),!.
obtenirCoup(_,_,_) :-
	write('Soit une IA n\'a pas renvoyé son coup, soit vous avez choisi un mauvais ID d\'IA'),
	nl,
	write('Veuillez choisir entre 2=iaAleatoire et 3=iaMinimax'),
	nl,
	fail,
	!.
