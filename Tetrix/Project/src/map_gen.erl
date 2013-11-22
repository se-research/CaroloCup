-module(map_gen).

-behaviour(gen_server).

%% Internal functions
-export([say/2]).
-compile(export_all).

%% API
-export([start_link/0, node_ahead/1, add_frame/3, road_side/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(XSCALE, 300/10.0).
-define(YSCALE, 420/20.0).

-record(state, {node_ahead, road_side, frame_data, matrix_id, camera_matrix}).
-recor(dash_line, {center_point, box, points, dash_before, dash_after}).

-include("../include/offsetCalculation.hrl").

init([]) ->
    say("init", []),
    
    {ok, ID} = ets:file2tab("include/undistort.txt"),
    %%Camera_Matrix = read_cm_file("include/camera_matrix.txt"),

    ets:new(dash_lines, [set, named_table]),

    % Dummy values for the state 
    {ok, #state{road_side = right, node_ahead = {{0,0},{0,0},{0,0}},
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

handle_cast({add_frame, {{Dashes, Line_ID}, CarPos}}, State) ->
    
    %% NewPoints = translate(State#state.matrix_id, State#state.camera_matrix, Points , []),

    NewDashes = translate_dashes(State#state.matrix_id, Dashes, []),
    
    case NewDashes of 
	[] ->
	    {noreply, State};
	[_] ->
	    {noreply, State};
	[H,T] ->
	    P1 = hd(H#dash_line.points),
	    P3 = lists:nth(3, T#dash_line.points),
	    P2 = center_point(lists:nth(3, H#dash_line.points), hd(TH#dash_line.points)),
	    Node_List = [offsetCalculation:calculate_offset_list(Line_ID, 
								 ?LaneAdjacent, 
								 H#dash_line.points),
			 offsetCalculation:calculate_offset_list(Line_ID, 
								 ?LaneAdjacent, 
								 [P1,P2,P3]),
			 offsetCalculation:calculate_offset_list(Line_ID, 
								 ?LaneAdjacent, 
								 T#dash_line.points)],
	    car_ai:start(Node_List),			 
	    {noreply, State#state{node_ahead = Node_List, frame_data = Node_List }};
	_ ->
	    Node_List = calculate_offsets(Line_ID, ?LaneAdjacent, NewDashes, []),
	    car_ai:start(Node_List),
	    {noreply, State#state{node_ahead = Node_List, frame_data = Node_List }}
    end;

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

translate_points(ID, [Point | T] , Buff) ->
    case translate_point(ID, Point) of
	[] ->
	    translate_points(ID, T, Buff);
	NewPoint ->
	    translate_points(ID, T, Buff ++ [NewPoint])
    end;
translate_points(_,[], Buff) ->
    Buff.

translate_dash(ID, {CenterPoint, {R1,R2,R3,R4} , {Bottom, Center, Top}}) ->
    #dash_line{center_point = translate_point(ID, CenterPoint), 
	       box = {translate_point(ID,R1),
		      translate_point(ID,R2),
		      translate_point(ID,R3),
		      translate_point(ID,R4)},
	       points = [translate_point(ID,Bottom),
			 translate_point(ID,Center),
			 translate_point(ID,Top)], 
	       dash_before = undef, dash_after = undef}.

translate_dashes(ID, [Dash | T], Buff) ->
    translate_dashes(ID, T, Buff ++ [ translate_dash(ID, Dash) ]); 
translate_dashes(_,[],Buff) ->
    Buff.

translate_point(ID, Point) ->
    case ets:lookup(ID , Point) of
	[] ->
	    [];
	[{_,NewPoint}] ->
	    NewPoint
    end.


calculate_offsets(InputLane, OutputType, [Dash | T] , Buff) ->
    calculate_offsets(InputLane, OutputType, T , Buff ++ 
			  [offsetCalculation:calculate_offset_list(InputLane, 
								   OutputType, 
								   Dash#dash_line.points)]
		     );
calculate_offsets(_, _, [] , Buff) ->
    Buff.





bird_transform(Camera_Matrix , {X, Y}) ->
    {CM1 , CM2 , CM3 , CM4 , CM5 , CM6 , CM7, CM8 , CM9} = Camera_Matrix,
    W = 1 / (CM7 * X + CM8 * Y + CM9),

    U = W * X,
    V = W * Y,
  
    ResX = CM1 * U + CM2 * V + CM3 * W,
    ResY = CM4 * U + CM5 * V + CM6 * W,

    [{((ResX - 376) * ?XSCALE), ((480 - ResY) * ?YSCALE)}].

read_cm_file(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    case io:get_line(Device, "") of
        eof  -> file:close(Device);
        Line ->	T = lists:map(fun(X) -> list_to_float(X) end, string:tokens(Line -- "\n" , ",")),
		list_to_tuple(T)
    end.


center_point({X1,Y1}, {X2,Y2}) ->
    {(X1 +X2)/2, (Y1+Y2)/2}.
