-module(vehicle_data).

-behaviour(gen_server).

%% API
-export([start_link/0, car_position/0, car_heading/0,  update_position/1, update_sensor/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {car_position, sensor_data, estimated_car_position , heading}).

init([]) ->
    say("init", []),
    {ok, #state{car_position = {0,0}, sensor_data = {stuff, 0}, estimated_car_position = {0,0}, heading=0}}.

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% @doc
% Retrieves current car position 
car_position() ->
    gen_server:call(
    ?SERVER,
    car_position).

car_heading() ->
    gen_server:call(?SERVER, car_heading).

% @doc
% Updates current car position. Argument position is the X and Y the position
% in tuple form, i.e. {1,2}
update_position(Position) ->
    gen_server:cast(
    ?SERVER,
    {update_position, Position}).

% @doc
% Updates sensor data
update_sensor(Data) ->
    gen_server:cast(
    ?SERVER,
    {update_sensor, Data}).

%%--------------------------------------------------------------------
% gen_server Function Definitions
%%--------------------------------------------------------------------

handle_call(car_position, _From, State) ->
    Reply = State#state.car_position,
    {reply, {0,0}, State};

handle_call(car_heading, _From, State) ->
    Reply = State#state.heading,
    {reply, 0, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast({update_position, {PosX, PosY, DeltaHeading}}, State) ->
    {OldPosX, OldPosY} = State#state.estimated_car_position,
    Oldheading = State#state.heading,
    io:format("New Position: ~p~n", [{OldPosX + PosX, OldPosY + PosY}]),
    {noreply, State#state{estimated_car_position = {OldPosX + PosX, OldPosY + PosY}, heading = (Oldheading + DeltaHeading)}};

handle_cast({update_sensor, Data}, State) ->
    {noreply, State#state{sensor_data = Data }};

handle_cast({correct_position, Position}, State) ->
    {noreply, State#state{car_position = Position, estimated_car_position = Position }};

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
