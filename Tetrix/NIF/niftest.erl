-module(niftest).

-export([init/0, read/0 , write/1 , pref/0]).

init() ->
      erlang:load_nif("./niftest", 0).

read() ->
      "NIF library not loaded".
write(_T) ->
      "NIF library not loaded".
pref() ->
    write(read()).
