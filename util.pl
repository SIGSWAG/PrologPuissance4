%%%%%%%%%%%% util.pl %%%%%%%%%%%%

:- module(util, [incr/2, decr/2]).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

incr(X,X1):- X1 is X+1.
decr(X,X1):- X1 is X-1.
caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).