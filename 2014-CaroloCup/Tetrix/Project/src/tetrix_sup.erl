
-module(tetrix_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_W(I, Type), {I, {I, start, []}, permanent, 2000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %supervisor:start_link(car_ai, ?CHILD(car_ai, worker)),
    {ok, { {one_for_one,5,10},[?CHILD(vehicle_data, supervisor),
			       ?CHILD(map_gen, supervisor), 
			       ?CHILD_W(tetrix_status, worker), 
			       ?CHILD(cunit, supervisor), 
			       ?CHILD_W(currentPos, worker),
			       ?CHILD_W(terminal, worker),
			       ?CHILD_W(image_proc, worker) 
			       ]}}.
%            ?CHILD_W(car_ai, worker) , ?CHILD(hardware_data, worker),
%            ?CHILD(image_proc, worker), ?CHILD(position_calc, worker)]}}.
    

