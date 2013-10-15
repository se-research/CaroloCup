-module(test_offsets).
-export([testing_offsets/0]).
-include("offsetCalculation.hrl").

testing_offsets()->
    offsetCalculation:start_link(),
    Nodes = generate_nodes(),
    testing_offsets(Nodes).

testing_offsets([])->
    ok;
testing_offsets([H | T]) ->
    OffsetList = offsetCalculation:calcOffsetList(?InputLaneL, ?InputLaneR, H),
    io:format("Input Array:~p~n", [H]),    
    io:format("Nodelist: ~p~n", [OffsetList]),
    testing_offsets(T). 

generate_nodes() ->
    Data = lists:seq(1,10),
    [ [{X,X}, {X+1, X+1},{X+2, X+2}]  || X <- Data].
