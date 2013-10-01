-module(image_proc).

-export([start/0, init/1]).


start() ->
    State = [],
    spawn(imgproc, init, [State]).

init(State) ->
    %% initialize 
    process(State).

process(State) ->
    %% query frame
    %% get car position
    %% get road side
    %% process image
    %% send valid data
    %% process(State)
    ok.

