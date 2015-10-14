:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
% to read Request parameters
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_cors)).
% for json
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
% http_reply_from_files is here
:- use_module(library(http/http_files)).
:- [run].

:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(files, '/f', []).




% ROUTING
:- http_handler('/', helloAction, []).
:- http_handler('/init', initAction, []).
:- http_handler('/selectPlayers', selectPlayersAction, []).
:- http_handler('/playFromIA', playFromIAAction, []).

% this serves files from the directory assets
% under the working directory
:- http_handler(files(.), filesAction, [prefix]).
:- http_handler('/game', indexAction, []).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).


run_game(Request) :-
    run().

% ACTIONS %
% return the html page of the game
indexAction(Request) :-
    http_reply_from_files('web/pages', [], Request).
indexAction(Request) :-
    http_404([], Request).    

% say hello, to test if server is running
helloAction(Request) :-
    format('Content-type: text/plain~n~n'),
    format('Hello world ! Server is running').

% initialyze the game
% Response : a list of the available players
initAction(Request) :-
    init,
    findall([X,Y],getTypeJoueurString(X,Y), Z),
    reply_json(json{correct:true, players:Z}).

% serve any file from assets
filesAction(Request) :-
    http_reply_from_files('web/assets', [], Request).
filesAction(Request) :-
    http_404([], Request).

% set the player selected
% Response tell what clolor has the players
selectPlayersAction(Request) :-
    findall(X,getTypeJoueurString(X,Y), TypeJoueurs),
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

playFromIAAction(Request) :-
    joueurCourant(CouleurJCourant,TypeJoueur),
    aQuiDemanderCoup(CouleurJCourant,TypeJoueur,'',X),
    placerJeton(X,Y,CouleurJCourant),
    isItTheEnd(X,Y,CouleurJCourant, Status),
    reply_json(json{correct:true, gameStatus:Status, colPlayed:X, rowPlayed:Y}).
% case : win
isItTheEnd(Coup,Y,CouleurJCourant, Status) :-
    gagne(Coup,Y,CouleurJCourant),
    Status = 'win'.
% case : draw
isItTheEnd(Coup,Y,CouleurJCourant, Status) :-
    not(coupPossible),
    Status = 'draw'.
% case : continue
isItTheEnd(Coup,Y,CouleurJCourant, Status) :-
    changerJoueur,
    Status = 'continue'.

getParam(Request) :-
    http_parameters(Request,
                    [ name(Name, []),
                      sex(Sex, [oneof([male,female])]),
                      birth_year(BY, [between(1850,10000)])
                    ]),
    % register_user(Name, Sex, BY),
    say_perdu(Name, Sex, BY).


say_perdu(Name, Sex, BY) :-
        format('Content-type: text/plain~n~n'),
        format('Alors on s apelle ~w ?~n On est ~w ?~n On est est en ~w?', [Name, Sex, BY]).

