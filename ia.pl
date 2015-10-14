%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :- nbColonnes(NBCOLONNES), Coup is random(NBCOLONNES)+1.