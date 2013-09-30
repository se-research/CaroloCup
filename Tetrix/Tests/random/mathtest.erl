-module(mathtest).

%%-export([start/0,init/0]).
-compile(export_all).

start() ->
    spawn(mathtest, init, []).

init() ->
    calc().

calc() ->
    %%math:atan((1-2)/(3-4)) + 
    %%math:pi()/2 - (math:pi()/2 * (2-1)/(2-1)),
    test:hello(),
    gen_server:cast(testserv, done). 


t(X,Y) ->
    case catch math:tan(X) of
	R ->
	     io:format("Result ~p ~n",[R])
    end.

