-module(hardware_data).

-export([start_link/0, init/1, get_sensor_data/0]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start_link() ->
    State = [],
    Pid = spawn(?SERVER, init, [State]),
    register(?SERVER, Pid),
    {ok, Pid}.

% @doc
% Retrieves data from sensors, sends them to vehicle_data server
get_sensor_data() ->
    ?SERVER ! get_sensor_data,
    ok.

%%--------------------------------------------------------------------
% Callback Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts the module
init(State) ->
    say("init", []),
    update(State).

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

update(State) ->
    receive
        get_sensor_data ->
            %% TODO: read sensors one by one/NIFS

            %% send sensor data to  vehicle_data server, TODO: using dummy data         
            Data = {stuff, 10},
            vehicle_data:update_sensor(Data)
    end,
    update(State).

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
