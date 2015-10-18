%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
:- module(ia, [iaAleatoire/1]).

:- use_module(jeu).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :- nbColonnes(NBCOLONNES), Coup is random(NBCOLONNES)+1, coupValide(Coup).
iaAleatoire(Coup) :- iaAleatoire(Coup).
