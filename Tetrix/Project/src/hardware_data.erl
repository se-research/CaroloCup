-module(hardware_data).

-export([start/0, init/1]).


start() ->
    State = [],
    spawn(hardware_data, init, [State]).

init(State) ->
    %% initialize 
    update(State).

update(State) ->
    %% read sensors one by one
    %% send to vehicle data
    %% update(NewState)
    ok.
