%%%%%%%%%%%% util.pl %%%%%%%%%%%%

:- module(util, [incr/2, decr/2]).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% incr/2(+X, -X1)
% unifie X1 à X+1
% vrai pour X1 = X+1
incr(X,X1):- X1 is X+1.


% decr/2(+X, -X1)
% unifie X1 à X-1
% vrai pour X1 = X-1
decr(X,X1):- X1 is X-1.









