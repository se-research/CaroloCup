-module(tetrix_status).

%-behaviour(gen_server).

%% API
-export([start/0, init/1]).

%% gen_server callbacks
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
%-define(Port, 8091).

start()->
  %net_kernel:start(['node1@arch', longnames]),
  State = [],
  Pid = spawn(?SERVER, init, [State]),
  register(shell, Pid),
  {ok, Pid}.

init(State) ->
  net_kernel:start(['node1', shortnames]),
  loop().

loop()->
  receive
    {establish_connection, From} ->
        io:format("in the node1 loop",[]),
        From ! {origin, self()};
    {check_availability, From} ->
        %io:format("rec'd random message in check_availability",[]),
        From ! {ok, available};
    _ ->
        io:format("rec'd random message in tetrix_status",[])
  end,
  loop().


%init() ->
%    say("init", []),
    %{ok, LSock} = gen_tcp:listen(?Port, [{active, true}]),
%    {ok, ListenSocket} = gen_tcp:listen(?Port, [{active,true}, binary]),
%    io:format("ListenSocket on server: ~p~n", [ListenSocket]),
%    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
%    io:format("AcceptSocket on server: ~p~n", [AcceptSocket]),
%    query().
    
    %{ok, ok}. 

%query() ->
%    receive
%        {tcp, Port, Message} ->
%            io:format("rec'd message: ~p~n_", [binary_to_term(Message)]),
%            {From, Request} = binary_to_term(Message), 
%            case Request of
%                checkStatus -> 
%                    io:format("in the checkStatus clause", []),
%                    From ! {ok, running}
%            end
%            gen_tcp:send(Port, "hello tetrix back")
%    end,
%    query().
 

% @doc
% Starts server
%start_link() ->
%    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
% gen_server Function Definitions
%%--------------------------------------------------------------------

%handle_call(checkTetrixAppStatus, From, State) ->
%    Reply = {ok, running},
%    {reply, Reply, State};

%handle_call(_Request, _From, State) ->
%    Reply = ok,
%    {reply, Reply, State}.

%%--------------------------------------------------------------------

%handle_cast({setSpeed, Speed}, State) ->
%    {noreply, State};

%handle_cast(_Msg, State) ->
%    {noreply, State}.

%%--------------------------------------------------------------------


%handle_info({tcp, _, Message}, State) ->
%    io:format("handled tcp message: ~p~n", [Message]);
%handle_info(_Info, State) ->
    % Unexpected messages, i.e. out-of-band
%    error_logger:info_msg("Unexpected message:~p~n", [_Info]),
%    {noreply, State}.

%%--------------------------------------------------------------------

%terminate(Reason, _State) ->
%    error_logger:info_msg("terminating:~p~n", [Reason]),
%    ok.

%%--------------------------------------------------------------------

%code_change(_OldVsn, State, _Extra) ->
%    say("code_change ~p, ~p, ~p", [ _OldVsn, State, _Extra]),
%    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

%checkTetrixAppStatus()->
%    gen_server:call(?SERVER, checkTetrixAppStatus).
