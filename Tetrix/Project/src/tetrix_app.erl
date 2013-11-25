-module(tetrix_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, stop/0]).

% Integration of unit testing
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tetrix_sup:start_link().

stop(_Reason) ->
  ok.

stop() ->
  imgproc_nif:deinit_camera(),
  init:stop().
