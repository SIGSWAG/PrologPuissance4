%%%%%%%%%%%% ia.pl %%%%%%%%%%%%

:- module(ia, [iaAleatoire/1]).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :- nbColonnes(NBCOLONNES), random_between(1,NBCOLONNES,Coup).