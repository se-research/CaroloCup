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

-record(state, {node_ahead, road_side, frame_data, matrix_id, camera_matrix, mode}).
-record(dash_line, {center_point, box, points, area, dash_before, dash_after}).

-include("../include/offsetCalculation.hrl").

init([]) ->
    say("init", []),
    
    {ok, ID} = ets:file2tab("include/undistort.txt"),
    %%Camera_Matrix = read_cm_file("include/camera_matrix.txt"),

    ets:new(dash_lines, [set, named_table]),

    % Dummy values for the state 
    {ok, #state{road_side = right, node_ahead = {{0,0},{0,0},{0,0}},
		matrix_id = ID , mode = start}}.

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
    %% query ets for node ahead
    Reply = State#state.node_ahead,
    {reply, Reply, State};

handle_call(road_side, _From, State) ->
    Reply = State#state.road_side,
    {reply, Reply, State};

handle_call(time_to_terminate, _From, State) ->
    ets:tab2file(dash_lines, "dashline"),
    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast({add_frame, {{Dashes, Line_ID}, {Car_Pos,Car_Heading}}}, State) ->

    
    

    Temp_Dashes = translate_dashes(State#state.matrix_id, Dashes, {Car_Pos,Car_Heading}, []),
    NewDashes = connect_dashes(Temp_Dashes, undef, []), 
    
    case State#state.mode of
	start ->
 	     insert_dashes(NewDashes);
 	recording ->
 	    Correction = calculate_correct_pos(NewDashes),  
 	    case Correction of
 		not_found ->
 		    ok;
 		{Center_Point = {Cx,Cy}, {Offset={Ox,Oy}, Delta_Angle}} ->
 		    Dashes_Needed = remove_dashes_before({Cx-Ox, Cy-Oy}, NewDashes),
  		    Moved_Dashes = move_dashes(Dashes_Needed, Correction, []),
  		    Orig_Dash = ets_lookup(Center_Point),
  		    Connected_Dashes = connect_dashes(Moved_Dashes, 
  						      Orig_Dash#dash_line.dash_before, []), 	    
  		    clean_ets_dashes(Center_Point),
 		    insert_dashes(Connected_Dashes),
 		    New_CarPos = move_point(Car_Pos, Correction),
 		    gen_server:cast(vehicle_data, {correct_position, New_CarPos})
 	    end
     end,
    %% query ets to get closest dash to each

    %% calculate exact car pos

    %% update dashes
    
    %% calculate offset nodes


     New_Dashes = translate_dashes(State#state.matrix_id, Dashes, {{4,-67},math:pi()/2},[]),
    
     case New_Dashes of 
         [] ->
             {noreply, State};
         [_] ->
             {noreply, State};
         [H,T] ->
             P1 = hd(H#dash_line.points),
             P3 = lists:nth(3, T#dash_line.points),
             P2 = center_point(lists:nth(3, H#dash_line.points), hd(T#dash_line.points)),
             {ok,[OP1]} = offsetCalculation:calculate_offset_list(Line_ID, ?LaneAdjacent, H#dash_line.points),
             {ok,[OP2]} = offsetCalculation:calculate_offset_list(Line_ID, ?LaneAdjacent, [P1,P2,P3]),
             {ok,[OP3]} = offsetCalculation:calculate_offset_list(Line_ID, ?LaneAdjacent, T#dash_line.points),
             
             Node_List = [OP1, OP2, OP3],
             car_ai:start(Node_List),                         
             {noreply, State#state{node_ahead = Node_List, frame_data = Node_List ,mode = recording}};
         _ ->
             Node_List = calculate_offsets(Line_ID, ?LaneAdjacent, NewDashes, []),
             car_ai:start(Node_List),
             {noreply, State#state{node_ahead = Node_List, frame_data = Node_List , mode = recording}}
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

translate_dash(ID, {CenterPoint, {{R1,R2,R3,R4} , {Bottom, Middle, Top}}}, {Car_Pos, Car_Heading}) ->
    Center = steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID, CenterPoint)),
    BottomLeft = steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,R1)),
    TopLeft = steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,R2)),
    TopRight = steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,R3)), 
    BottomRight = steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,R4)),
    #dash_line{center_point = Center, 
	       box = {BottomLeft, TopLeft, TopRight, BottomRight},
	       points = [steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,Bottom)),
			 steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,Middle)),
			 steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,Top))], 
	       area = calculate_box_area(BottomLeft, TopLeft, TopRight, BottomRight),
	       dash_before = undef, dash_after = undef}.

translate_dashes(ID, [Dash | T], {Car_Pos,Car_Heading} , Buff) ->
    translate_dashes(ID, T, {Car_Pos,Car_Heading}, 
		     Buff ++ [ translate_dash(ID, Dash, {Car_Pos,Car_Heading}) ]); 
translate_dashes(_,[],_,Buff) ->
    Buff.

translate_point(ID, Point) ->
    case ets:lookup(ID , Point) of
	[] ->
	    [];
	[{_,NewPoint}] ->
	    NewPoint
    end.


calculate_offsets(InputLane, OutputType, [Dash | T] , Buff) ->
    {ok,[P]} = offsetCalculation:calculate_offset_list(InputLane, OutputType, Dash#dash_line.points),
    calculate_offsets(InputLane, OutputType, T , Buff ++ [P]);
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

offset_point({X1,Y1}, {X2,Y2}) ->
    {X2-X1, Y2-Y1}.

closest_in_radius({CX,CY}, Radius) ->
    MatchSpec = match_spec(CX,CY,Radius),
    T = ets:select(dash_lines, MatchSpec),

    element(2,hd(ets:lookup(dash_lines, element(2,lists:min(T))))).


rect_angle({{X1,Y1},_,{X2,Y2},_}) ->
    math:atan2(Y2-Y1 , X2-X1).


calculate_correct_pos([Dash| T]) ->
    Corresponding_Dash = closest_in_radius(Dash#dash_line.center_point, 400),
    Offset = offset_point(Dash#dash_line.center_point , Corresponding_Dash#dash_line.center_point),
    Angle1 = rect_angle(Dash#dash_line.box),
    Angle2 = rect_angle(Corresponding_Dash#dash_line.box),
    Delta_Angle = Angle2 - Angle1,
    case {Dash#dash_line.area < 50 , Dash#dash_line.area > 30} of
	{true,true} ->
	    {Corresponding_Dash#dash_line.center_point, {Offset, Delta_Angle}};
	_ ->
	    calculate_correct_pos(T)
    end;
calculate_correct_pos([]) ->
    not_found.
    


calculate_box_area({X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}) ->
    abs((X1*Y2 - X2*Y1 + X2*Y3 - X3*Y2 + X3*Y4 - X4*Y3 + X4*Y1 - X1*Y4) / 2).


move_dashes([Dash|T], Correction, Buff) ->
    move_dashes(T, Correction, Buff ++ [move_dash(Dash,Correction)]);
move_dashes([],_,Buff) ->
    Buff.

%%    Dash = ets_lookup(Point),
%%    New_Dash = move_dash(Dash, Correction),
%%    ets:insert(dash_lines, {Point, New_Dash}),
%%    case Dash#dash_line.dash_after of
%%	undef ->
%%	    ok;
%%	After ->
%%	    move_dashes(After, Correction)
%%    end.

move_dash(#dash_line{center_point = Center, 
		     box = {P1,P2,P3,P4}, 
		     points = [BottomP, CenterP, TopP]} , Correction) ->
    

    #dash_line{center_point = move_point(Center, Correction) ,
	       box = {move_point(P1, Correction),
		      move_point(P2, Correction),
		      move_point(P3, Correction),
		      move_point(P4, Correction)},
	       points = [move_point(BottomP, Correction),
			 move_point(CenterP, Correction),
			 move_point(TopP, Correction)],
	       area = calculate_box_area(move_point(P1, Correction),
					 move_point(P2, Correction),
					 move_point(P3, Correction),
					 move_point(P4, Correction))}.
	
					 
move_point({X,Y},{{Cx,Cy},{{Dx,Dy}, Rotation}}) ->
    
    Distance = getDistance({X,Y}, {Cx,Cy}),
    Angle = getAng({Cx,Cy}, {X,Y}),
    NewX = (Distance * math:cos(Angle+Rotation)) + Cx + Dx,
    
    NewY = (Distance * math:sin(Angle+Rotation)) + Cy + Dy,
    

    
    %% OffX = X + Dx,
    %% OffY = Y + Dy,
    %% Distance = getDistance({OffX,OffY} , {Cx,Cy}),
    
    %% NewX = Cx + (Distance * math:cos(Angle)) ,
    %% NewY = Cy + (Distance * math:sin(Angle)) ,
    {NewX, NewY}.


ets_lookup(Point) ->
    case ets:lookup(dash_lines, Point) of
	[] ->
	    ok;
	[{Point, Value}] ->
	    Value
    end.



getAng({X1,Y1} , {X2,Y2}) -> 
    math:atan2(Y2-Y1,X2-X1).
					
getDistance({X1,Y1} , {X2,Y2}) ->
    math:sqrt(math:pow(Y2-Y1,2) + math:pow(X2-X1,2)).
   


connect_dashes([H1,H2|T] , Before , Buff) ->
    connect_dashes([H2|T], H1#dash_line.center_point , 
		   Buff ++ [H1#dash_line{dash_before = Before , 
					 dash_after = H2#dash_line.center_point}]);
connect_dashes([H], Before, Buff) ->
    	   Buff ++ [H#dash_line{dash_before = Before , 
					 dash_after = undef}].


remove_dashes_before({Cx,Cy},List = [H|T]) ->
    case H#dash_line.center_point of
	{Cx,Cy} ->
	    List;
	_ ->
	    remove_dashes_before({Cx,Cy}, T)
    end;
remove_dashes_before(_,[]) ->
    well_fuck.

clean_ets_dashes(Point) ->
    Orig_Dash = ets_lookup(Point),
    ets:delete(dash_lines, Point),
    case Orig_Dash#dash_line.dash_after of
	undefined ->
	    ok;
	Next ->
	    clean_ets_dashes(Next)
    end.


insert_dashes([Dash|T]) ->
    ets:insert(dash_lines, {Dash#dash_line.center_point, Dash}),
    insert_dashes(T);
insert_dashes([]) ->
    ok.

match_spec(CX,CY,R) ->
[{{{'$1','$2'},'$3'},
  [{'andalso',{'<',{'+',{'*',{'-','$1',{const,CX}},
                             {'-','$1',{const,CX}}},
                        {'*',{'-','$2',{const,CY}},{'-','$2',{const,CY}}}},
                   {'*',{const,R},{const,R}}},
              {'>',{'+',{'*',{'-','$1',{const,CX}},{'-','$1',{const,CX}}},
                        {'*',{'-','$2',{const,CY}},{'-','$2',{const,CY}}}},
                   {'-',{'*',{const,R},{const,R}}}}}],
  [{{{'+',{'*',{'-','$1',{const,CX}},{'-','$1',{const,CX}}},
          {'*',{'-','$2',{const,CY}},{'-','$2',{const,CY}}}},
     {{'$1','$2'}}}}]}].

		  
