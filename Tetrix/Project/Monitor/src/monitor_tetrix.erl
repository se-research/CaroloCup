-module(monitor_tetrix).

%% API
-export([start/0]).

%% Internal exports
-export([init/1]).

-define(SERVER, ?MODULE). 

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start()->
  Host = get_hostname(),
  Pid = spawn(?SERVER, init, [Host]),
  register(shell, Pid),
  {ok, Pid}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

init(Host) ->
  net_kernel:start(['node2', shortnames]),
  loop(Host).

loop(Host)->

  case nodes() of
      [] ->
        case net_kernel:connect_node(list_to_atom("node1@" ++ Host) ) of
              false ->
                    timer:sleep(1000),
                    io:format("false: no connection\n",[]),
                    loop(Host);
              true ->
                  io:format("connection established\n",[])
        end,
        case rpc:multicall(nodes(), erlang, is_alive, []) of
            {[],[]} ->
                io:format("no nodes are alive\n",[]),
                os:cmd("erl -pa /ebin/ -sname node1 -setcookie nodes -noshell &"),
                timer:sleep(1000),
                loop(Host);
            {[true],_} ->
                io:format("Monitoring tetrix app",[])
        end;
      _ ->
        {shell, list_to_atom("node1@" ++ Host) } ! {check_availability, self()}
  end,

  {shell,list_to_atom("node1@" ++ Host) } ! {check_availability, self()},

  receive
  {ok, available} ->
        io:format("tetrix app is available!\n",[]),
        timer:sleep(1000),
        loop(Host)
  end.

%% @doc
%% Retrieves the hostmachines host name i.e. tetrix@odroid, hostname is odroid
get_hostname()->
  RawHost = os:cmd("hostname"),
  string:strip(RawHost,right,$\n).
