% @doc
% Provides offset calculations for lane markings
-module(offsetCalculation).

-include("../include/offsetCalculation.hrl").

-define (SERVER, ?MODULE).
 
%% API
-export([calculate_offset_list/3]).


% Wrapper method that calculating the offset list
calculate_offset_list(InputLane, OutputType, Input) ->
    case OutputType of
        ?AdjacentSideLine -> Offset = ?AdjustantLaneOpposite;
        ?LaneAdjacent -> Offset = ?OffsetLaneAdjacent;
        ?DashLine -> Offset = ?OffsetLaneThickness;
        ?LaneOpposite -> Offset = ?OffsetLaneOpposite;
        ?OppositeSideLine -> Offset = ?RoadThickness 
    end,
    calculate_offset_list(InputLane, Input, length(Input), 2, Offset, []). 

% Functions that calculates the NodeList mader up of offsets
calculate_offset_list(_InputLane, _Input, Counter, Counter,_Offset, NodeList) ->
    {ok, NodeList};
calculate_offset_list(InputLane, Input, InputSize, Counter,Offset, NodeList)
        when InputLane == ?InputLaneD ->
    NewOffset = calcOffset(
        element(1, lists:nth(Counter-1, Input)), 
        element(2, lists:nth(Counter-1, Input)),
        element(1, lists:nth(Counter, Input)),
        element(2, lists:nth(Counter, Input)),
        element(1, lists:nth(Counter +1, Input)),
        element(2, lists:nth(Counter +1, Input)),  
        (?RoadThickness/2) - Offset, 
        -1),
    calculate_offset_list(InputLane, Input, InputSize, Counter+1, Offset,
        lists:append(NodeList,[NewOffset]));

calculate_offset_list(InputLane, Input, InputSize, Counter,Offset, NodeList) ->
    NewOffset = calcOffset(
        element(1, lists:nth(Counter-1, Input)),
        element(2, lists:nth(Counter-1, Input)),
        element(1, lists:nth(Counter, Input)),
        element(2, lists:nth(Counter, Input)),
        element(1, lists:nth(Counter +1, Input)),
        element(2, lists:nth(Counter +1, Input)),
        Offset,
        InputLane),
    calculate_offset_list(InputLane, Input, InputSize, Counter+1, Offset,
        lists:append(NodeList,[NewOffset])).
    
% Calculates the offset knowing one of the lines with the help of atan2. We can
% check exactly if it is in a negative or passative direction from 180 degrees
calcOffset(X1, Y1, X2, Y2, X3, Y3, Offset, InputLane) ->
    NewOffsetAng = math:atan2((Y1 - Y2) + (Y2 - Y3), (X1 - X2) + (X2 - X3)) -
        (?PI/2),
    NewX = X2 + math:cos(NewOffsetAng)*Offset*InputLane,
    NewY = Y2 + math:sin(NewOffsetAng)*Offset*InputLane,
    {NewX, NewY}.

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
