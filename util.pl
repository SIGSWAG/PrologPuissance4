%%%%%%%%%%%% util.pl %%%%%%%%%%%%

:- module(util, [incr/2, decr/2]).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

incr(X,X1):- X1 is X+1.
decr(X,X1):- X1 is X-1.
caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).

addToList(X, L, [X|L]). 

ligne(1).
ligne(2).
%parcoursArbre

parcoursArbre(Pmax):- parcours(0,0,Pmax,[0]).

parcours(X, P, Pmax, L):- P==Pmax, nl, write('feuille'), print(L), assert(feuille(L, 3)).
parcours(X, P, Pmax, L) :- incr(P, P1), 
parcours(1, P1,Pmax, [1|L]), 
parcours(2, P1,Pmax,[2|L]), 
feuille([1|L], X1),feuille([2|L], X2), 
Max is X1+X2
, assert(feuille(L,Max)), print(L).