-module(pcbnif).

-compile(export_all).

-define(NIF_STUB, exit(nif_library_not_loaded)).

-on_load(init/0).

init() ->
     erlang:load_nif("./pcbnif", 0).


start_pcb() ->
    ?NIF_STUB.
stop_pcb(_) ->
    ?NIF_STUB.
set_leftLight(_,_) ->
    ?NIF_STUB.
set_rightLight(_,_) ->
    ?NIF_STUB.
set_brakeLight(_,_) ->
    ?NIF_STUB.
set_remoteLight(_,_) ->
    ?NIF_STUB.
set_beep(_,_) ->
    ?NIF_STUB.
set_display(_,_) ->
    ?NIF_STUB.
set_speed(_,_) ->
    ?NIF_STUB.
set_angle(_,_) ->
    ?NIF_STUB.
set_direction(_,_) ->
    ?NIF_STUB.
get_Switch1(_) ->
    ?NIF_STUB.
get_Switch2(_) ->
    ?NIF_STUB.
get_remoteX(_) ->
    ?NIF_STUB.
get_remoteY(_) ->
    ?NIF_STUB.
get_remoteStatus(_) ->
    ?NIF_STUB.
get_Voltage(_) ->
    ?NIF_STUB.
