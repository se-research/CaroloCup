-module(monitor_tetrix).

%% API
-export([start/0]).

%% Internal exports
-export([init/1]).

-define(SERVER, ?MODULE). 

-record(state, {host, currentPOS}).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start()->

  % Get the program going

  % Initilize the module
  Host = get_hostname(),
  State = #state{host = Host, currentPOS = 0},
  Pid = spawn(?SERVER, init, [State]),
  register(shell, Pid),
  {ok, Pid}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

init(State) ->
  %net_kernel:start(['node2', shortnames]),
  loop(State).

loop(State)->

  case nodes() of
      [] ->
        case net_kernel:connect_node(list_to_atom("node1@" ++ State#state.host) ) of
              false ->
                    timer:sleep(1000),
                    io:format("false: no connection\n",[]),
                    os:cmd(".././init_tetrix"), 
                    net_kernel:connect_node(list_to_atom("node1@" ++ State#state.host) ),
%                    os:cmd("cd ../;./run.sh &"), 
%                    timer:sleep(3000),
%                    net_kernel:connect_node(list_to_atom("node1@" ++ Host) ),
%                    os:cmd("erl -pa /ebin/ -sname node1 -setcookie nodes -noshell &"),
                    %os:cmd("erl -pa /ebin/ -sname node1 -setcookie nodes -noshell &"),
                    loop(State);
              true ->
                  io:format("connection established\n",[])
        end,
        case rpc:multicall(nodes(), erlang, is_alive, []) of
            {[],[]} ->
                 io:format("no nodes are alive\n",[]),
                 os:cmd(".././init_tetrix"), 
                 net_kernel:connect_node(list_to_atom("node1@" ++ State#state.host) ),
 
%                os:cmd("erl -pa /ebin/ -sname node1 -setcookie nodes -noshell &"),
                timer:sleep(1000),
                loop(State);
            {[true],_} ->
                io:format("Monitoring tetrix app",[])
%                loop(Host)
        end;
      _ ->
        {shell, list_to_atom("node1@" ++ State#state.host) } ! {check_availability, self()}
  end,

  {shell,list_to_atom("node1@" ++ State#state.host) } ! {check_availability, self()},

  receive
  {ok, available} ->
        io:format("tetrix app is available!\n",[]),
        timer:sleep(1000),
        loop(State)
  end.

%% @doc
%% Retrieves the hostmachines host name i.e. tetrix@odroid, hostname is odroid
get_hostname()->
  RawHost = os:cmd("hostname"),
  string:strip(RawHost,right,$\n).
