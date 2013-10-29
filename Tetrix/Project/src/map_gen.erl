-module(map_gen).

-behaviour(gen_server).

%% Internal functions
-export([say/2]).

%% API
-export([start_link/0, node_ahead/1, add_frame/3, road_side/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {node_ahead, road_side, frame_data, matrix_id, camera_matrix}).

-include("../include/offsetCalculation.hrl").

init([]) ->
    say("init", []),
    
    {ok, ID} = ets:file2tab("../include/undistort.txt"),
    Camera_Matrix = read_cm_file("../include/camera_matrix.txt"),

    % Dummy values for the state 
    {ok, #state{road_side = right,
		matrix_id = ID , camera_matrix = Camera_Matrix}}.

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
    NewPoints = translate(State#state.matrix_id, State#state.camera_matrix, Points , []),
    {ok, Node_List = [N1,N2,N3 | _]} = 
	offsetCalculation:calculate_offset_list(Line_ID, ?AdjacentSideLine, NewPoints),
 
    %% later when we have position we add nodes, right now will be replaced
    %% TODO??: add CarPos as current position in state record
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

translate(ID, Camera_Matrix, [Point | T] , Buff) ->
    case ets:lookup(ID , Point) of
	[] ->
	    translate(ID, Camera_Matrix, T, Buff);
	[{_,NewPoint}] ->
	    translate(ID, Camera_Matrix, T, Buff ++ bird_transform(Camera_Matrix, NewPoint))
    end;
translate(_,_,[], Buff) ->
    Buff.




bird_transform(Camera_Matrix , {X, Y}) ->
    {CM1 , CM2 , CM3 , CM4 , CM5 , CM6 , CM7, CM8 , CM9} = Camera_Matrix,
    W = 1 / (CM7 * X + CM8 * Y + CM9),

    U = W * X,
    V = W * Y,
  
    ResX = CM1 * U + CM2 * V + CM3 * W,
    ResY = CM4 * U + CM5 * V + CM6 * W,
  
    [{ResX, ResY}].

read_cm_file(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    case io:get_line(Device, "") of
        eof  -> file:close(Device);
        Line ->	T = lists:map(fun(X) -> list_to_float(X) end, string:tokens(Line -- "\n" , ",")),
		list_to_tuple(T)
    end.

