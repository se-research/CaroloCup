-module(hidnif).
-export([start_usb/0,stop_usb/1,set_speed/2,set_angle/2,set_display/2,set_brakeLight/2,
set_leftLight/2,set_rightLight/2,start/0,loop/2,get_accelSpeed/1,get_modeSwitch/1,get_remoteStatus/1,
get_Voltage/1,get_Current/1,get_Heading/1,get_IR0/1,get_IR1/1,get_ultraSonic/1]).


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
    Pid=self(),
    register(?MODULE, spawn(hidnif,loop,[Adress,Pid])).

loop(Adress,Pid)->
    receive
	{setSpeed,Value}->
	    set_speed(Value, Adress), 
            loop(Adress,Pid);
	{setAngle,Value}->
	    set_angle(Value, Adress), 
            loop(Adress,Pid);
	{setDisplay,Value}->
	    set_display(Value, Adress), 
            loop(Adress,Pid);
	{setBrakeLight,Value}->
	    set_brakeLight(Value, Adress), 
            loop(Adress,Pid);
	{setLeftLight,Value}->
	    set_leftLight(Value, Adress), 
            loop(Adress,Pid);
	{setRightLight,Value}->
	    set_rightLight(Value, Adress), 
            loop(Adress,Pid);
	{getAccelSpeed}->
	    Tmp=get_accelSpeed(Adress),
	    Pid ! {curSpeed,Tmp}, 
            loop(accelSpeed,Pid);
	{getModeSwitch}->
	    Tmp=get_modeSwitch(Adress),
	    Pid ! {modeSwitch,Tmp}, 
            loop(Adress,Pid);
	{getRemoteStatus}->
	    Tmp=get_remoteStatus(Adress),
	    Pid ! {remoteStatus,Tmp}, 
            loop(Adress,Pid);
	{getVoltage}->
	    Tmp=get_Voltage(Adress),
            Pid ! {voltage,Tmp}, 
            loop(Adress,Pid);
	{getCurrent}->
	    Tmp=get_Current(Adress),
	    Pid ! {current,Tmp}, 
            loop(Adress,Pid);
	{getHeading}->
	    Tmp=get_Heading(Adress),
            Pid ! {heading,Tmp}, 
            loop(Adress,Pid);
	{getIR0}->
	    Tmp=get_IR0(Adress),
            Pid ! {iR0,Tmp}, 
            loop(Adress,Pid);
	{getIR1}->
	    Tmp=get_IR1(Adress),
            Pid ! {iR1,Tmp}, 
            loop(Adress,Pid);
	{getUltraSonic}->
	    Tmp=get_ultraSonic(Adress),
            Pid ! {ultraSonic,Tmp}, 
            loop(Adress,Pid);
        {stop}->
	    stop_usb(Adress)
    end.
	



