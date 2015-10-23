%%%%%%%%%%%% webserver.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).   % to read Request parameters
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).         % for json
:- use_module(library(http/json_convert)).      
:- use_module(jeu).
:- use_module(ia).
:- use_module(ihm).
:- use_module(eval).

:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(files, '/f', []).


%%%%%%%%%%%%%%%%
%%  Routing   %%
%%%%%%%%%%%%%%%%

:- http_handler('/', helloAction, []).
:- http_handler('/init', initAction, []).
:- http_handler('/selectPlayers', selectionnerJoueurAction, []).
:- http_handler('/playFromIA', tourIAAction, []).
:- http_handler('/validHumanPlay', validerTourHumain, []).
% this serves files from the directory assets
% under the working directory
:- http_handler(files(.), fichierAction, [prefix]).
:- http_handler('/game', indexAction, []).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

% ACTIONS %
% return the html page of the game
indexAction(Request) :-
    http_reply_from_files('web/pages', [], Request).
indexAction(Request) :-
    http_404([], Request).    

% say hello, to test if server is running
helloAction(_) :-
    format('Content-type: text/plain~n~n'),
    format('Hello world ! Server is running').

% initialyze the game
% Response : a list of the available players
initAction(_) :-
    initJeu,
    retractall(joueurCourant(_,_)),
    retractall(autreJoueur(_,_)),
    findall([X,Y],getTypeJoueurString(X,Y), Z),
    reply_json(json{correct:true, players:Z}).

% serve any file from assets
fichierAction(Request) :-
    http_reply_from_files('web/assets', [], Request).
fichierAction(Request) :-
    http_404([], Request).

% set the player selected
% Response tell what clolor has the players
selectionnerJoueurAction(Request) :-
    % findall(X,getTypeJoueurString(X,Y), TypeJoueurs),
    http_parameters(Request,
    % oneof(TypeJoueurs) ne marche pas ....atom_number('123', X)
    % on obtient une erreur bad request : Parameter "joueur1" must be one of "1" or "2".  Found "1"
        [ joueur1(TypeJoueur1, []),
          joueur2(TypeJoueur2, [])
        ]),
    random_select(TypeJoueurR,[TypeJoueur1,TypeJoueur2],[TypeJoueurJ|_]),
    atom_number(TypeJoueurR, TypeJoueurRInteger),
    atom_number(TypeJoueurJ, TypeJoueurJInteger),
    assert(joueurCourant(rouge,TypeJoueurRInteger)),
    assert(autreJoueur(jaune,TypeJoueurJInteger)),
    reply_json(json{correct:true, rouge:TypeJoueurR, jaune:TypeJoueurJ}).

validerTourHumain(Request) :-
    http_parameters(Request,[ col(Col, [])]),
    atom_number(Col, Colonne),
    joueurCourant(CouleurJCourant,_),
    placerJeton(Colonne, Ligne, CouleurJCourant),
    statutJeu(Colonne,Ligne,CouleurJCourant, Statut),
    reply_json(json{correct:true, gameStatus:Statut, colPlayed:Colonne, rowPlayed:Ligne}),
    !.
validerTourHumain(_) :-
    reply_json(json{correct:true, gameStatus:invalid}).

tourIAAction(_) :-
    joueurCourant(CouleurJCourant,TypeJoueur),
    obtenirCoup(CouleurJCourant,TypeJoueur,'',Colonne),
    placerJeton(Colonne,Ligne,CouleurJCourant),
    statutJeu(Colonne,Ligne,CouleurJCourant, Statut),
    reply_json(json{correct:true, gameStatus:Statut, colPlayed:Colonne, rowPlayed:Ligne}).

% case : win
statutJeu(Colonne,Ligne,CouleurJCourant, 'win') :-
    gagne(Colonne,Ligne,CouleurJCourant).
% case : draw
statutJeu(_,_,_, 'draw') :-
    not(coupPossible).
% case : continue
statutJeu(_,_,_, 'continue') :-
    changerJoueur.

% permet d'appeler l'ihm ou les IAs pour récupérer le coup suivant
% 2==IA aleatoire
obtenirCoup(_,2,_,Coup) :-
    iaAleatoire(Coup).
% 3==minimax
obtenirCoup(_,3,_,Coup) :-
    donneCoup(Coup).