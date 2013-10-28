-module(image_proc).

-export([start/0, init/1, process_image/0]).

-define(SERVER, ?MODULE).

-include("../include/offsetCalculation.hrl").

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start() ->
    State = [],
    Pid = spawn(?SERVER, init, [State]).

process_image() ->
    ?SERVER ! process_image,
    ok.

%%--------------------------------------------------------------------
% Callback Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts the module
init(State) ->
    %% initialize 
    say("init", []),
    process(State).

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

process(State) ->

    %% get car position vehicle_data
    Car_Pos = vehicle_data:car_position(),
    
    %% query frame
    %%{ok, ImgRef} = imgproc_nif:get_pic(),
    Img_Points = [1,2,3],

    %% get road side 
    Side = map_gen:road_side(),
    
    %% TODO: process image and generate valid data
    
    %% send valid data to map_server, using dummy values
    map_gen:add_frame(Img_Points, ?InputLaneD , Car_Pos),
    
    %% make delay
    process(State).

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
