-module(cunit).

-behaviour(gen_server).

%% API
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

init([]) ->
    {ok,hidnif:start()},
    Address = hidnif:start_usb(),
    case Address of
	0 ->
	    io:format("~nError: Car is not found !! Please Connect The Car.~n"),
	    ignore;
	_ ->
	    {ok, Address}
    end.

% @doc
% Starts server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
% gen_server Function Definitions
%%--------------------------------------------------------------------

handle_call(getAccelSpeed, From, State) ->
    Reply = hidnif:get_accelSpeed(State),
    {reply, Reply, State};

handle_call(getModeSwitch, From, State) ->
    Reply = hidnif:get_modeSwitch(State),
    {reply, Reply, State};

handle_call(getRemoteStatus, From, State) ->
    Reply = hidnif:get_remoteStatus(State),
    {reply, Reply, State};

handle_call(getVoltage, From, State) ->
    Reply = hidnif:get_Voltage(State),
    {reply, Reply, State};

handle_call(getCurrent, From, State) ->
    Reply = hidnif:get_Current(State),
    {reply, Reply, State};

handle_call(getHeading, From, State) ->
    Reply = hidnif:get_Heading(State),
    {reply, Reply, State};

handle_call(getIR0, From, State) ->
    Reply = hidnif:get_IR0(State),
    {reply, Reply, State};

handle_call(getIR1, From, State) ->
    Reply = hidnif:get_IR1(State),
    {reply, Reply, State};

handle_call(getUltraSonic, From, State) ->
    Reply = hidnif:get_ultraSonic(State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast({setSpeed, Speed}, State) ->
    hidnif:set_speed(Speed, State),
    {noreply, State};

handle_cast({setSteering, Angle}, State) ->
    hidnif:set_angle(Angle, State),
    {noreply, State};

handle_cast({setDisplay, Info}, State) ->
    hidnif:set_display(Info, State),
    {noreply, State};

handle_cast({setBrakeLight, Status}, State) ->
    hidnif:set_brakeLight(Status, State),
    {noreply, State};

handle_cast({setLeftLight, Status}, State) ->
    hidnif:set_leftLight(Status, State),
    {noreply, State};

handle_cast({setRightLight, Status}, State) ->
    hidnif:set_rightLight(Status, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    % Unexpected messages, i.e. out-of-band
    error_logger:info_msg("Unexpected message:~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(Reason, _State) ->
    error_logger:info_msg("terminating:~p~n", [Reason]),
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

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

setSpeed(Speed) ->
    gen_server:cast(?SERVER, {setSpeed, Speed}).     

setSteering(Steering) ->
    gen_server:cast(?SERVER, {setSteering, Steering}).     

setDisplay(Info) ->
    gen_server:cast(?SERVER, {setDisplay, Info}).

setBrakeLight(Status) ->
    gen_server:cast(?SERVER, {setBrakeLight, Status}).

setLeftLight(Status) ->
    gen_server:cast(?SERVER, {setLeftLight, Status}).

setRightLight(Status) ->
    gen_server:cast(?SERVER, {setRightLight, Status}).

getAccelSpeed() ->
    gen_server:call(?SERVER, getAccelSpeed).

getModeSwitch() ->
    gen_server:call(?SERVER, getModeSwitch).

getRemoteStatus() ->
    gen_server:call(?SERVER, getRemoteStatus).

getVoltage() ->
    gen_server:call(?SERVER, getVoltage).

getCurrent() ->
    gen_server:call(?SERVER, getCurrent).

getHeading() ->
    gen_server:call(?SERVER, getHeading).

getIR0() ->
    gen_server:call(?SERVER, getIR0).

getIR1() ->
    gen_server:call(?SERVER, getIR1).

getUltraSonic() ->
    gen_server:call(?SERVER, getUltraSonic).
