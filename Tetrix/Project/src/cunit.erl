-module(cunit).

-behaviour(gen_server).

%% API
-export([start_link/0, speed/1, steering/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {speed, steering}).

init([]) ->
    say("init", []),
    {ok, #state{speed = 0, steering = 0}}.

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% @doc
% Argument is desired speed
speed(Speed) ->
    gen_server:cast(
    ?SERVER,
    {speed, Speed}).     

% @doc
% Argument is desired steering
steering(Steering) ->
    gen_server:cast(
    ?SERVER,
    {steering, Steering}).     

%%--------------------------------------------------------------------
% gen_server Function Definitions
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast({speed, Speed}, State) ->

    io:format("~nspeed rec'd:~p~n" ,[Speed]), 
    % TODO: Adjust car speed/ using a NIF?
    {noreply, State};

handle_cast({steering, Steering}, State) ->

    io:format("~nsteering rec'd:~p~nsteering" ,[Steering]), 
    % TODO: Adjust car steering/ using a NIF? 
    {noreply, State};

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
