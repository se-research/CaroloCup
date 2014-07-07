
-module(testserv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    F = fun(X) -> 
		mathtest:start()
	end,
    lists:foreach(F, lists:seq(1,100001)),

    {ok, {100000,erlang:now()}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, {1,{_,TS,TM}}) ->
    {_,S,M} = erlang:now(),
    io:format("Time ~p ~n", [{S-TS, M-TM}]),
    {stop, normal, []};	      
handle_cast(_Msg, {N,T}) ->
    {noreply, {N-1,T}}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
