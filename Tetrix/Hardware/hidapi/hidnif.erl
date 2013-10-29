-module(hidnif).
-export([start_usb/0,stop_usb/1,set_speed/2,set_angle/2,set_display/2,set_brakeLight/2,set_leftLight/2,set_rightLight/2,start/0,loop/1]).


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

start() ->
    erlang:load_nif("./hidnif", 0),
    Adress=start_usb(),
    register(?MODULE, spawn(hidnif,loop,[Adress])).

loop(Adress)->
    receive
	{setspeed,Value}->
	    set_speed(Value, Adress), 
            loop(Adress);
	{setangle,Value}->
	    set_angle(Value, Adress), 
            loop(Adress);
	{setdisplay,Value}->
	    set_display(Value, Adress), 
            loop(Adress);
	{setbrakeLight,Value}->
	    set_brakeLight(Value, Adress), 
            loop(Adress);
	{setleftLight,Value}->
	    set_leftLight(Value, Adress), 
            loop(Adress);
	{setrightLight,Value}->
	    set_rightLight(Value, Adress), 
            loop(Adress);
        {stop}->
	    stop_usb(Adress)
    end.
	



