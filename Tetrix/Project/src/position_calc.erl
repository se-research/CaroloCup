-module(position_calc).

-export([start/0, init/1]).


start() ->
    State = [],
    spawn(position_calc, init, [State]).

init(State) ->
    %% initialize 
    locate(State).

locate(State) ->
    %% read mouse or other sensor data
    %% calculate car position
    %% send position to vehicle data 
    %% locate(NewState)
    ok.

