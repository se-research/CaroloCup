-module(test_offsetCalculation).
-include("../include/offsetCalculation.hrl").
-include_lib("eunit/include/eunit.hrl").

% Prevents this code from being included as compiled code in ebin
-ifdef(TEST).

% Helper function: starts the offsetCalculation module
initialize_start_link() ->
    offsetCalculation:start_link().

% Helper function: rounds the list of offsets
round_offsets(Offsets) ->
    [{round(X), round(Y) } || {X, Y} <- Offsets].

% Tests calcOffsetList, with an input of a length of 3 
calcOffsetList_1_test()->
    initialize_start_link(),
    Input = [{1,1},{2,3},{4,5}],
    Offsets = offsetCalculation:calcOffsetList(?InputLaneL, ?InputLaneR,
        Input),
    %Answer = [{round(X), round(Y) } || {X, Y} <- Offsets],
    Answer = round_offsets(Offsets),
    io:format("answer: ~p", [Answer]),
    ?assertEqual([{18, -9}], Answer).

% Tests calcOffsetList, with an input of a length of 5 
calcOffsetList_2_test() ->
    Input = [{4,4}, {10,20}, {30,50}, {6,3}, {4,9}], 
    Offsets = offsetCalculation:calcOffsetList(?InputLaneL, ?InputLaneR,
        Input),
    Answer = round_offsets(Offsets),
    ?assertEqual([{27, 10},{11,55},{-11,14}], Answer).

% Tests calcOffsetList, if the input provided is less than 2 in length. If it
% is, it returns thep previous nodeList/state
calcOffsetList_3_test() ->
    Input = [{2,0}], 
    Offsets = offsetCalculation:calcOffsetList(?InputLaneL, ?InputLaneR,
        Input),
    Answer = round_offsets(Offsets),
    ?assertEqual([{27, 10},{11,55},{-11,14}], Answer).

-endif.

