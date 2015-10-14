:- module(utils, [aleatoire/2, incr/2, decr/2, caseVide/2]).

% incr/2(+X, -X1)
% unifie X1 à X+1
% vrai pour X1 = X+1
incr(X,X1):- X1 is X+1.
% decr/2(+X, -X1)
% unifie X1 à X-1
% vrai pour X1 = X-1
decr(X,X1):- X1 is X-1.
% caseVide/2(+X, +Y)
% verifie si la case est vide
% vrai si la case n'a pas été remplie
caseVide(X,Y) :- nonvar(X),nonvar(Y),not(case(X,Y,_)).
