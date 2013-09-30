-module(imgproc_nif).

-compile(export_all).

-define(NIF_STUB, exit(nif_library_not_loaded)).

-on_load(init/0).

init() ->
      erlang:load_nif("./imgproc_nif", 0).

get_pic() ->
    ?NIF_STUB.

show_pic(F) ->
    ?NIF_STUB.


%% INTERNAL

    
    
