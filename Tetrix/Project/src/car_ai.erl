-module(car_ai).

-export([start/0, init/1]).


start() ->
    State = [],
    spawn(car_ai, init, [State]).

init(State) ->
    calculate(State).

calculate(State) ->
    %% get node list ahead
    %% get car position
    %% calculate heading and speed
    %% send data to cunit
    %% calculate(NewState)
    ok.

