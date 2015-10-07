%%%%%%%%%%%%%%%%%%%%%%%%
%% Méthodes à appeler %%
%%%%%%%%%%%%%%%%%%%%%%%%
afficher :-
	findall(_, afficherPlateau(Y), _).

demandeCoup(Joueur, Message, Coup) :-
    write(Message), nl, write(Joueur), saisirCoup(Coup).

gagne(Joueur) :-
	write('Le joueur '), write(Joueur), write(' gagne.').

demandeTypeDeJeu(TypeDeJeu) :-
    write('   --- Puissance 4 --- '), nl,
    write('    1. Jouer au jeu avec un humain     '), nl,
    write('                         '), nl,
	write(' ----------------------- '), nl,
    write('Saisissez votre choix :'), nl,
    read(TypeDeJeu), integer(TypeDeJeu).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Méthodes "privées" %%
%%%%%%%%%%%%%%%%%%%%%%%%

% principe : on parcourt la base de faits et pour chaque case on affiche une couleur (ou pas)
afficherPlateau(Y) :-
	between(1,6,Y1),
	Y is 7-Y1,
	findall(_, afficherLigne(X,Y), _),
	nl.

afficherLigne(X,Y) :-
	between(1,6,X),
	afficherCase(X,Y).

afficherCase(X,Y) :-
	case(X,Y,A),
	write(A), !. % si on trouve une case dans la base des faits, on ne veux pas afficher une case vide, donc on arrete la recherche pour ce X et Y, d'où le "!"
afficherCase(_,_) :-
	write(.).

saisirCoup(Coup) :-
	write('Veuillez saisir votre coup : '),
	read(Coup).