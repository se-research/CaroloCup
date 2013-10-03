-module(hidnif).

%%-export([init/0, read/0 , write/1 , pref/0, test/0, test_1000/2, get_]).

-compile(export_all).


init() ->
      erlang:load_nif("./hidnif", 0).

open_handle() ->
      "NIF library not loaded".

close_handle(F) ->
      "NIF library not loaded".

