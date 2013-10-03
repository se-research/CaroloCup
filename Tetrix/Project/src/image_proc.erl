-module(image_proc).

-export([start/0, init/1]).


start() ->
    State = [],
    spawn(imgproc, init, [State]).

init(State) ->
    %% initialize 
    process(State).

process(State) ->
    CarPos = {0,0}, %% gen_server:call(vehicle_data, get_pos)
    Side = right,
    List = imgproc_nif:get_pic(),
    gen_server:cast(map_gen, {add_frame, List, CarPos}),
    process(State).
        
    %% query frame
    %% get car position
    %% get road side
    %% process image
    %% send valid data
    %% process(State)
    %%ok.

