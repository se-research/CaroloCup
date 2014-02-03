-module(monitor_tetrix).

%% API
-export([start/0]).

%% Internal exports
-export([init/1, startup_tetrix/0]).

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
                    case startup_tetrix() of
                     ok ->
                       net_kernel:connect_node(list_to_atom("node1@" ++ State#state.host) )
                    end,
 
     %               os:cmd(".././init_tetrix"), 
     %               net_kernel:connect_node(list_to_atom("node1@" ++ State#state.host) ),
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
                 %os:cmd(".././init_tetrix"), 
                 case startup_tetrix() of
                    ok ->
                      net_kernel:connect_node(list_to_atom("node1@" ++ State#state.host) )
                 end,
 
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

startup_tetrix() ->
  os:cmd(".././init_tetrix"), 
  RawAnswer = os:cmd("./src/tetrix_available.sh"),  
  Answer = string:strip(RawAnswer,right,$\n),

  case Answer of
    "0" ->
        io:format("looping, trying to start tetrix app", []),
        startup_tetrix();
    "1" ->
        ok 
  end. 


