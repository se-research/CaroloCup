-module(car_ai).

%% API
-export([start/1, init/1]).

%% Internal exports
%-export([calculate/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts the module
start(Points) ->
    State = Points,
    Pid = spawn(?SERVER, init, [State]),
    {ok, Pid}.

% @doc
% Calculates speed and steering, and sends it to cunit server

%%--------------------------------------------------------------------
% Callback Definitions 
%%--------------------------------------------------------------------

init(State) ->
    %% say("init", []),
    calculate(State),
    ok.

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

calculate(Node_List) ->
    %% Get car position from vehicle data, in form of {X, Y}
    Car_Position = {4,-67}, %%vehicle_data:car_position(), 
    Car_Heading = math:pi() / 2,

    Len = length(Node_List),
    P1 = hd(Node_List),
    P2 = lists:nth(round(Len/2), Node_List),
    P3 = lists:nth(Len, Node_List),
    



    %% Get 3 node lists ahead, i.e. Node1 = {5,6}, etc              
    %% {P1,P2,P3} = map_gen:node_ahead(Car_Position), 
    %% io:format("Nodes ahead"),
    %% TODO: calculate heading and speed
    Steering = steering:calculate(P1, P2, P3, Car_Position, Car_Heading),
    
    %% io:format("STEERING ANGLE : ~p~n" , [Steering]),

    %% send desired speed to cunit 
    %% TODO: dummy values
    cunit:setSpeed(2),
    
    %% send desired steering to cunit
    %% TODO: dummy values    ok.
    cunit:setSteering(round((Steering * 180.0) / math:pi())),
    io:format("Steering: ~p~n", [round((Steering*180.0)/ math:pi())]).
%%    Speed = cunit:getAccelSpeed(),
%%    Heading = cunit:getHeading(),
%%    {ok, Log} = file:open("Log.txt", [read, append]),
%%    {Mega, Sec, Micro} = erlang:now(),
%%    io:fwrite(Log, "~p,~p,~p,~p,~p,~p,~p,~n", [Mega, Sec, Micro, Speed, Heading, Steering, round((Steering*180.0)/ math:pi())]),
%%    file:close(Log).    
    



    %% timer:sleep(20),
 
    %% calculate().

%% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

