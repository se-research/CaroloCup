-module(car_ai).

%% API
-export([start_link/0, init/1, calculate_speed_steering/0]).

%% Internal exports
%-export([calculate/1]).

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
% Calculates speed and steering, and sends it to cunit server
calculate_speed_steering() ->
    ?SERVER ! calculate_speed_steering,
    ok.

%%--------------------------------------------------------------------
% Callback Definitions 
%%--------------------------------------------------------------------

init(State) ->
    say("init", []),
    calculate(State),
    ok.

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

calculate(State) ->
    receive 
        calculate_speed_steering ->

            %% Get car position from vehicle data, in form of {X, Y}
            Car_Position = vehicle_data:car_position(), 

            %% Get 3 node lists ahead, i.e. Node1 = {5,6}, etc              
            {Node1, Node2, Node3} = map_gen:node_ahead(Car_Position), 

            %% TODO: calculate heading and speed

            %% send desired speed to cunit 
            %% TODO: dummy values
            cunit:speed(5),

            % send desired steering to cunit
            %% TODO: dummy values
            cunit:steering(4) 
    end,
    calculate(State).

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

