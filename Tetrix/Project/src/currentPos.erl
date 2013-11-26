-module(currentPos).

-export([start/0, init/1]).

-compile(export_all).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start() ->
    State = {0, 0},
    Pid = spawn(?SERVER, init, [State]),
    register(?SERVER, Pid),
    {ok,Pid}.

%%--------------------------------------------------------------------
%% Callback Definitions 
%%--------------------------------------------------------------------

%% @doc
%% Starts the module
init(State) ->
    %% initialize 
    say("init", []),
    loop(State).

%%--------------------------------------------------------------------
%% Internal functions Definitions 
%%--------------------------------------------------------------------

loop({PrevHal, PrevHeading}) ->
    receive
	{hal, []} ->
	    loop({PrevHal, PrevHeading}); 
	{hal,  Bytes} ->
	    CurrHal = list_to_integer(Bytes),
	%    CurrHal = cunit:getAccelSpeed(),
	    CurrHeading = cunit:getHeading(),
            say("Hal: ~p", [CurrHal]),
	   % say("Positions: ~p", [calculatePos(PrevHal, CurrHal, PrevHeading, CurrHeading)]),     
vehicle_data:update_position(calculatePos(PrevHal, CurrHal, PrevHeading, CurrHeading)),
	    loop({CurrHal,CurrHeading})
    end.
	    
   

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

calculatePos(PrevHal, CurrHal, PrevHeading, CurrHeading)->

    NewHal = (CurrHal - PrevHal)*(1000/57),

    case (NewHal < 0) of
	true ->
	    DeltaDistance = NewHal + 1000;
	false ->
	    DeltaDistance = NewHal
    end,

    Angle = CurrHeading - PrevHeading,

    DeltaHeading = normalized((Angle*math:pi()/180))*(180/math:pi()),

    PosX = DeltaDistance * ( math:cos( (CurrHeading * math:pi()/180) )),
    
    PosY = DeltaDistance * ( math:sin( (CurrHeading * math:pi()/180) )),
    
    {PosX, PosY, DeltaHeading}.

normalized(Angle)->
    case {Angle > math:pi() , Angle < -math:pi()} of
	{true, _} ->
	    NewAngle = normalize(Angle, (-2.0 * math:pi()) );
	{_,true} ->
	    NewAngle = normalize(Angle, (2.0 * math:pi()) );
	_ ->
	    NewAngle = Angle
    end,
    case abs(NewAngle) == math:pi() of
	true ->
	    0.0;
	_ ->
	    NewAngle
    end.

normalize(Angle, MyPI) ->
    case {Angle > math:pi() , Angle < -math:pi()} of
	{false,false} ->
	    Angle;
	_ ->
	    normalize(Angle+MyPI, MyPI)
    end.
