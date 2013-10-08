-module(image_proc).

-export([start_link/0, init/1, process_image/0]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start_link() ->
    State = [],
    Pid = spawn(?SERVER, init, [State]),
    register(?SERVER, Pid),
    {ok, Pid}.

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
    receive
        process_image ->

            %% get car position vehicle_data
            CarPos = vehicle_data:car_position(),

            %% query frame
            %{ok, ImgRef} = imgproc_nif:get_pic(),
            ImgRef = [1,2,3],

            %% get road side 
            Side = map_gen:road_side(),

            %% TODO: process image and generate valid data

            %% send valid data to map_server, using dummy values
            map_gen:add_frame("frame points", {8,8})
    end,
    process(State).

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
