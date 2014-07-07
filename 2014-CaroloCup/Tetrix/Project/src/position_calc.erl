-module(position_calc).

%% API
-export([start_link/0, init/1, calculate_car_position/0]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts the module
start_link() ->
    State = [],
    Pid = spawn(?SERVER, init, [State]),
    register(?SERVER, Pid),
    {ok, Pid}.

% @doc
% Calculates car position, and sends it to vehicle_data server
calculate_car_position() ->
    ?SERVER ! calculate_car_position,
    ok.

%%--------------------------------------------------------------------
% Callback Definitions 
%%--------------------------------------------------------------------

init(State) ->
    %% initialize 
    say("init", []),
    locate(State).

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

locate(State) ->
    receive 
        calculate_car_position ->    
            %% TODO: read mouse or other sensor data

            %% TODO: calculate car position

            %% send position to vehicle data
            %% TODO: Dummy values
            vehicle_data:update_position({4,5})
    end,
    locate(State).

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
