-module(map_gen).

-behaviour(gen_server).

%% Internal functions
-export([say/2, read_matrix/2]).

%% API
-export([start_link/0, node_ahead/1, add_frame/3, road_side/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {node_ahead, road_side, frame_data, matrix_id}).

-include("../include/offsetCalculation.hrl").

init([]) ->
    say("init", []),
    
    {ok, ID} = ets:file2tab("../include/undistort.txt"),
    

    % Dummy values for the state 
    {ok, #state{node_ahead = {{0,0}, {1,1}, {2,2}}, road_side = right,
        frame_data = {"frame points", {8,7} }, matrix_id = ID }}.

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% @doc
% Argument is a tuple containing X and Y coords, i.e. Car_Pos = {5,6}
node_ahead(Car_Pos) ->
    gen_server:call(
    ?SERVER,
    {node_ahead, Car_Pos}).

% @doc
% Adds frame data from image processing. Arguments are Points detected, and Car
% position 
add_frame(Points, Lane_ID, Car_Pos) ->
    gen_server:cast(
      ?SERVER,
      {add_frame, {{Points, Lane_ID}, Car_Pos}}).

% @doc
% Retrieves the side of the road that the vehicle is on
road_side() ->
    gen_server:call(
      ?SERVER,
      road_side).

%%--------------------------------------------------------------------
% gen_server Function Definitions
%%--------------------------------------------------------------------

handle_call({node_ahead,{CarX,CarY}}, _From, State) ->

    % TODO: generate node ahead with {X,Y} values, using dummy values
    Reply = State#state.node_ahead,
    {reply, Reply, State};

handle_call(road_side, _From, State) ->
    Reply = State#state.road_side,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast({add_frame, {{Points, Line_ID}, CarPos}}, State) ->
    {ok, Node_List = [N1,N2,N3 | _]} = 
	offsetCalculation:calculate_offset_list(Line_ID, ?AdjacentSideLine, Points),
 
    %% later when we have position we add nodes, right now will be replaced
    %% TODO??: add CarPos as current position in state record
    car_ai:start(),
    {noreply, State#state{node_ahead = {N1, N2, N3}, frame_data = Node_List }};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    % Unexpected messages, i.e. out-of-band
    error_logger:info_msg("Unexpected message:~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    error_logger:info_msg("terminating:~p~n", [?MODULE]),
    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    say("code_change ~p, ~p, ~p", [ _OldVsn, State, _Extra]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
