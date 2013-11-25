-module(image_proc).

-export([start/0, init/1, process_image/0]).

-define(SERVER, ?MODULE).

-include("../include/offsetCalculation.hrl").

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start() ->
    State = 0,
    Pid = spawn(?SERVER, init, [State]),
    {ok, Pid}.

process_image() ->
    ?SERVER ! process_image,
    ok.

%%--------------------------------------------------------------------
%% Callback Definitions 
%%--------------------------------------------------------------------

%% @doc
%% Starts the module
init(State) ->
    %% initialize 
    imgproc_nif:init(),
    say("init", []),
    process(State).

%%--------------------------------------------------------------------
%% Internal functions Definitions 
%%--------------------------------------------------------------------

process(State) ->
    %% get car position vehicle_data
    Car_Pos = vehicle_data:car_position(),
    Car_Heading = vehicle_data:car_heading(),
    %% Side = map_gen:road_side(),
    
    %% query frame
    case imgproc_nif:get_pic() of
	{ok, ImgRef} ->
	    Processed = imgproc_nif:process_pic(ImgRef, State),
	    case Processed of
		not_found ->
		    not_found;
		_ ->
		    map_gen:add_frame(Processed, ?InputLaneD , {Car_Pos,Car_Heading})
	    end;
	_ ->
	    not_found
    end,

    timer:sleep(30),
    process(State+1).

%% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
