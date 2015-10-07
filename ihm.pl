case(1,1,r).

afficher :- findall(_, afficher_plateau(Y), _).

afficher_plateau(Y) :- between(1,6,Y1), Y is 7-Y1, findall(_, afficher_ligne(X,Y), _), nl.

afficher_ligne(X,Y) :- between(1,6,X), afficher_case(X,Y).

afficher_case(X,Y) :- case(X,Y,A), write(A), !.
afficher_case(_,_) :- write(.).