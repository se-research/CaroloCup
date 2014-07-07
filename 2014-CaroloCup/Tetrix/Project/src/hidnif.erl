-module(hidnif).
-compile(export_all).


start_usb() ->
    exit("NIF library not loaded").

stop_usb(_Address) ->
    exit("NIF library not loaded").

set_speed(_Speed, _Address) ->
    exit("NIF library not loaded").

set_angle(_Angle, _Address) ->
    exit("NIF library not loaded").

set_display(_Text, _Address) ->
    exit("NIF library not loaded").

set_brakeLight(_bValue, _Address) ->
    exit("NIF library not loaded").

set_leftLight(_lValue, _Address) ->
    exit("NIF library not loaded").

set_rightLight(_rValue, _Address) ->
    exit("NIF library not loaded").

get_accelSpeed(_Address) ->
    exit("NIF library not loaded"). 

get_modeSwitch(_Address) ->
    exit("NIF library not loaded"). 

get_remoteStatus(_Address) ->
    exit("NIF library not loaded"). 

get_Voltage(_Address) ->
    exit("NIF library not loaded"). 

get_Current(_Address) ->
    exit("NIF library not loaded"). 

get_Heading(_Address) ->
    exit("NIF library not loaded").

get_IR0(_Address) ->
    exit("NIF library not loaded").

get_IR1(_Address) ->
    exit("NIF library not loaded").

get_ultraSonic(_Address) ->
    exit("NIF library not loaded").

cause_segmentation_fault() ->
    exit("NIF library not loaded").

start() ->
    erlang:load_nif("./ebin/hidnif", 0).
