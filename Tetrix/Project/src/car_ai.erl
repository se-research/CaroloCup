-module(car_ai).

%% API
-export([start/0, init/1]).

%% Internal exports
%-export([calculate/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts the module
start() ->
    State = [],
    Pid = spawn(?SERVER, init, [State]),
    {ok, Pid}.

% @doc
% Calculates speed and steering, and sends it to cunit server

%%--------------------------------------------------------------------
% Callback Definitions 
%%--------------------------------------------------------------------

init(State) ->
    say("init", []),
    calculate(),
    ok.

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

calculate() ->
    %% Get car position from vehicle data, in form of {X, Y}
    Car_Position = vehicle_data:car_position(), 
    Car_Heading = math:pi() / 2,
 
    %% Get 3 node lists ahead, i.e. Node1 = {5,6}, etc              
    {P1,P2,P3} = map_gen:node_ahead(Car_Position), 
    
    %% TODO: calculate heading and speed
    Steering = steering:calculate(P1, P2, P3, Car_Position, Car_Heading),

    %% send desired speed to cunit 
    %% TODO: dummy values
    cunit:speed(1),
    
    %% send desired steering to cunit
    %% TODO: dummy values
  
    cunit:steering(Steering),
    
    timer:sleep(20),
 
    calculate().

%% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

