-module(testpcb).

-export([start/0, init/0]).

-export([remote/2]).

-define(SERVER, ?MODULE).



start() ->
    Pid = spawn(?SERVER, init, []),
    {ok, Pid}.


init() ->
    io:format("CAR AI INITNT~n", []),
    
    P = pcbnif:start_pcb(),
    pcbnif:set_speed(90,P),
    timer:sleep(1000),
    remote(P,0),
    ok.

 remote(Pcb_Address,Count) ->
     case Count of
	1000 ->
		pcbnif:set_leftLight(1, Pcb_Address),
		 pcbnif:set_rightLight(1, Pcb_Address),
		NewCount=Count+1;
	2000 ->
		 pcbnif:set_leftLight(0, Pcb_Address),
		 pcbnif:set_rightLight(0, Pcb_Address),
		 NewCount=0;
	_ ->	
		NewCount=Count+1
	end,
     RemoteOn = pcbnif:get_remoteStatus(Pcb_Address),
     pcbnif:set_brakeLight(1, Pcb_Address),
     pcbnif:set_beep(0, Pcb_Address),
     pcbnif:set_display("Tetrix test",Pcb_Address),
%%    io:format("REMOTE STATUS : ~p~n" , [RemoteOn]),
     case RemoteOn of
     	1 ->
      	    pcbnif:set_remoteLight(1, Pcb_Address),
      	    UpDown= pcbnif:get_remoteY(Pcb_Address),
      	    RightLeft=pcbnif:get_remoteX(Pcb_Address),
      	    case  RightLeft of
     	    	1 -> Ang=135;
      		2 -> Ang=55;
      		_ -> Ang=90
      	    end,
     	    pcbnif:set_angle(Ang, Pcb_Address),
      	    pcbnif:set_speed(20, Pcb_Address),
      	    pcbnif:set_direction(UpDown, Pcb_Address),
		timer:sleep(1),
        	remote(Pcb_Address,NewCount);
	_ -> 
	    pcbnif:set_remoteLight(0, Pcb_Address),
	    pcbnif:set_direction(4, Pcb_Address),
	   pcbnif:set_angle(90, Pcb_Address),
	    timer:sleep(1),
            remote(Pcb_Address,NewCount)
	end.

