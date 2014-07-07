%%%-------------------------------------------------------------------
%%% @author Khashayar <khashayar@localhost.localdomain>
%%% @copyright (C) 2013, Khashayar
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2013 by Khashayar <khashayar@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(niftestserv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
init([]) ->
    %%{ok, Ptr} = niftest:get_pic(),
    
   %% niftest:start(Ptr,2),
   %% niftest:start(Ptr,3),
%%    niftest:start(Ptr,4),
    {ok, {[],{}}}.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(start, {State,_}) ->
    {ok, Ptr} = niftest:get_pic(),
    Time = erlang:now(),
    niftest:start(Ptr,1),
    niftest:start(Ptr,2),
    niftest:start(Ptr,3),
    niftest:start(Ptr,4),
    {noreply, {State, Time}};
handle_cast({add,List,4}, {State,{_,TS,TM}}) ->
    {_,S,M} = erlang:now(),
    io:format("Time ~p ~n", [{S-TS, M-TM}]),
    {noreply, {State ++ List, {}}};
handle_cast({add,List,_}, {State,Time}) ->
    {noreply, {State ++ List, Time}};
handle_cast(show, {State,Time}) ->
    io:format("Typing : ~p~n",[length(State)]),
    {noreply, {State,Time}};
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    io:format("SHUTTING" ,[]),
    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
