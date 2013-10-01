-module(cunit_nif).

-compile(export_all).

-define(NIF_STUB, exit(nif_library_not_loaded)).

%%-on_load(init/0).

init() ->
      erlang:load_nif("./cunit_nif", 0).

open_handle() ->
    ?NIF_STUB.

close_handle(H) ->
    ?NIF_STUB.


%% INTERNAL





    
    
