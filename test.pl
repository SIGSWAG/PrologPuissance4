test :- T=1,getT(T,X),write(X).

test2 :- T=2,getT(T,X),write(X).

getT(1,X) :- X='un',!.
getT(2,X) :- X='deux'.