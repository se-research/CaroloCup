-module(test).

-export([init/0, hello/0,distance/1]).

init() ->
      erlang:load_nif("./test", 0).

hello() ->
      "NIF library not loaded".

distance(0) ->
    ok;
distance(N) ->
    math:atan((1-2)/(3-4)) + 
	math:pi()/2 - (math:pi()/2 * (2-1)/(2-1)),
    distance(N-1).

