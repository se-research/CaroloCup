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

start() ->
    erlang:load_nif("./hidnif", 0),
    Adress=start_usb(),
    register(?MODULE, spawn(hidnif,loop,[Adress])).

loop(Adress)->
    receive
	{setSpeed,Value}->
	    set_speed(Value, Adress), 
            loop(Adress);
	{setAngle,Value}->
	    set_angle(Value, Adress), 
            loop(Adress);
	{setDisplay,Value}->
	    set_display(Value, Adress), 
            loop(Adress);
	{setBrakeLight,Value}->
	    set_brakeLight(Value, Adress), 
            loop(Adress);
	{setLeftLight,Value}->
	    set_leftLight(Value, Adress), 
            loop(Adress);
	{setRightLight,Value}->
	    set_rightLight(Value, Adress), 
            loop(Adress);
	{getAccelSpeed,Value}->
	    get_accelSpeed(Adress), 
            loop(Adress);
	{getModeSwitch,Value}->
	    io:format("~d~n", [get_modeSwitch(Adress)]), 
            loop(Adress);
	{getRemoteStatus,Value}->
	    get_remoteStatus(Adress), 
            loop(Adress);
	{getVoltage,Value}->
	    get_Voltage(Adress), 
            loop(Adress);
	{getCurrent,Value}->
	    get_Current(Adress), 
            loop(Adress);
	{getHeading,Value}->
	    get_Heading(Adress), 
            loop(Adress);
	{getIR0,Value}->
	    get_IR0(Adress), 
            loop(Adress);
	{getIR1,Value}->
	    get_IR1(Adress), 
            loop(Adress);
	{getUltraSonic,Value}->
	    get_ultraSonic(Adress), 
            loop(Adress);
        {stop}->
	    stop_usb(Adress)
    end.
	



