%%%%%%%%%%%%%%%%%%%%%%%%
%% Méthodes à appeler %%
%%%%%%%%%%%%%%%%%%%%%%%%
afficher :-
	findall(_, afficherPlateau(_), _).

demandeCoup(Joueur, Message, Coup) :- nl, write(Message), nl, write('['), write(Joueur), write('] '), saisirCoup(Coup).

afficherGagnant(Joueur) :-
	write('Le joueur '), write(Joueur), write(' gagne.').

demandeTypeDeJeu(TypeDeJeu) :-
    write('   --- Puissance 4 ---'), nl,
    write('    1. Jouer au jeu avec un humain'), nl,
    nl, nl,
	write(' ----------------------- '), nl,
    write('Saisissez votre choix :'), nl,
    read(TypeDeJeu), integer(TypeDeJeu).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Méthodes "privées" %%
%%%%%%%%%%%%%%%%%%%%%%%%

% principe : on parcourt la base de faits et pour chaque case on affiche une couleur (ou pas)
afficherPlateau(Y) :-
	nbLignes(NbLignes),
	between(1,NbLignes,Y1),
	Y is 7-Y1,
	findall(_, afficherLigne(_,Y), _),
	nl.

afficherLigne(X,Y) :-
	nbColonnes(NbColonnes),
	between(1,NbColonnes,X),
	afficherCase(X,Y).

afficherCase(X,Y) :- case(X,Y,rouge), write(r), !.
afficherCase(X,Y) :- case(X,Y,jaune), write(j), !.
afficherCase(_,_) :- write(.).

saisirCoup(Coup) :-
	write('Veuillez saisir votre coup : '),
	read(Coup).