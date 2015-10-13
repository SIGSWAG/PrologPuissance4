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
% this serves files from the directory assets
% under the working directory
:- http_handler(files(.), filesAction, [prefix]).

:- http_handler('/game', indexAction, []).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).


run_game(Request) :-
    run().

% ACTIONS %
indexAction(Request) :-
     http_reply_from_files('web/pages', [], Request).
indexAction(Request) :-
      http_404([], Request).    
    
helloAction(Request) :-
    format('Content-type: text/plain~n~n'),
    format('Hello world ! Server is running').

initAction(Request) :-
    init(),
    % cors_enable,
    reply_json(json{correct:true}).

filesAction(Request) :-
     http_reply_from_files('web/assets', [], Request).
filesAction(Request) :-
      http_404([], Request).

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

