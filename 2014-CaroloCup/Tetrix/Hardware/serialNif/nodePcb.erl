-module(nodePcb).

-define(REMOTESENSE,40).
-define(ALARAMVOLATAGE,10.5).
-define(LEDINTERVAL,500).
-define(REMOTERIGHT,55).
-define(REMOTELEFT,135).
-define(REMOTESPEED,90).
-define(SERVOOFFVALUE,10).

-export([start/0, init/0]).

-export([loop/10,timeLoop/2,switch/6]).


start() ->
    register(?MODULE, Pid =spawn(?MODULE,init,[])),
    {ok, Pid}.


init() ->
    io:format("CAR PCB INITNT~n", []),
    Addr = pcbnif:start_pcb(),
    pcbnif:set_display("Tetrix test     Current mode: 0",Addr),
    spawn(?MODULE,timeLoop,[self(),0]),
    spawn(?MODULE,switch,[Addr,self(),0,0,0,0]),
    loop(Addr,0,0,0,0,0,0,0,0,0),
    ok.

 loop(Pcb_Address,ClientPid,State,PrevRemoteState,Count,Left,Right,CountRemote,LightInterval,SetMode) ->
	Voltage=pcbnif:get_Voltage(Pcb_Address),
	case Voltage > ?ALARAMVOLATAGE of
		true ->
			pcbnif:set_beep(0, Pcb_Address);
		false  ->
			pcbnif:set_beep(1, Pcb_Address)
	end,
	RemoteOn = pcbnif:get_remoteStatus(Pcb_Address),
	case RemoteOn of
		1 ->
			NewCountRemote=CountRemote+1;
		_ ->
			NewCountRemote=0
	end,
	case NewCountRemote >= ?REMOTESENSE of
        true ->
	    case LightInterval of
            	?LEDINTERVAL ->
                 	pcbnif:set_remoteLight(0, Pcb_Address),
                 	NewLightInterval = LightInterval+1;
            	(?LEDINTERVAL*2) ->
                 	pcbnif:set_remoteLight(1, Pcb_Address),
                 	NewLightInterval = 0;
            	_ ->
                 	NewLightInterval = LightInterval+1
            end,
            UpDown= pcbnif:get_remoteY(Pcb_Address),
            RightLeft=pcbnif:get_remoteX(Pcb_Address),
            case  RightLeft of
                1 -> Ang=?REMOTELEFT;
                2 -> Ang=?REMOTERIGHT;
                _ -> Ang=90
            end,
            pcbnif:set_angle(Ang, Pcb_Address),
            pcbnif:set_speed(?REMOTESPEED, Pcb_Address),
            pcbnif:set_direction(UpDown, Pcb_Address),
	    pcbnif:set_leftLight(0, Pcb_Address),
	    pcbnif:set_rightLight(0, Pcb_Address),
	    timer:sleep(1),
            loop(Pcb_Address,ClientPid,State,1,0,Left,Right,?REMOTESENSE,NewLightInterval,SetMode);
        false ->
			pcbnif:set_remoteLight(0, Pcb_Address),
			case Left of
				1->
					pcbnif:set_leftLight(State, Pcb_Address);
				_->
					pcbnif:set_leftLight(0, Pcb_Address)
			end,
			case Right of
				1->
					pcbnif:set_rightLight(State, Pcb_Address);
				_->
					pcbnif:set_rightLight(0, Pcb_Address)
				end,
			case PrevRemoteState of
				1 ->    pcbnif:set_angle(90, Pcb_Address),
					pcbnif:set_direction(4, Pcb_Address);
				_ -> ok		
			end,
			case SetMode of
				10 -> 
					pcbnif:set_angle(90, Pcb_Address),
					pcbnif:set_direction(4, Pcb_Address);
				_ ->  ok
			end,
			receive
				[changeState,NewState]->
					case Count of
						?SERVOOFFVALUE -> 
							pcbnif:set_angle(1, Pcb_Address),
							NewCount=0;
						_ -> 
							NewCount=Count+1
					end, 
					loop(Pcb_Address,ClientPid,NewState,0,NewCount,Left,Right,NewCountRemote,0,SetMode);
				[setClientPid,Pidclient]->
					loop(Pcb_Address,Pidclient,State,0,Count,Left,Right,NewCountRemote,0,SetMode);
				[changeMode,0] ->
					pcbnif:set_display("Tetrix test     Confirm mode 0",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,10);
				[changeMode,1] ->
					pcbnif:set_display("Tetrix test     Confirm mode 1",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,10);
				[changeMode,2] ->
					pcbnif:set_display("Tetrix test     Confirm mode 2",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,10);
				[changeMode,3] ->
					pcbnif:set_display("Tetrix test     Confirm mode 3",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,10);
				[changeMode,4] ->
					pcbnif:set_display("Tetrix test     Confirm mode 4",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,10);
				[changeMode,5] ->
					pcbnif:set_display("Tetrix test     Confirm mode 5",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,10);
				[changeMode,6] ->
					pcbnif:set_display("Tetrix test     Confirm mode 6",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,10);
				[setMode,0] ->
					pcbnif:set_display("Tetrix test     Current mode: 0",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,0);
				[setMode,1] ->
					pcbnif:set_display("Tetrix test     Current mode: 1",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,1);
				[setMode,2] ->
					pcbnif:set_display("Tetrix test     Current mode: 2",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,2);
				[setMode,3] ->
					pcbnif:set_display("Tetrix test     Current mode: 3",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,3);
				[setMode,4] ->
					pcbnif:set_display("Tetrix test     Current mode: 4",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,4);
				[setMode,5] ->
					pcbnif:set_display("Tetrix test     Current mode: 5",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,5);
				[setMode,6] ->
					pcbnif:set_display("Tetrix test     Current mode: 6",Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,6);
				[angle,Angle]->
					case SetMode of
						10 -> pcbnif:set_angle(90, Pcb_Address);
						_ ->  pcbnif:set_angle(Angle, Pcb_Address)
					end,
					loop(Pcb_Address,ClientPid,State,0,0,Left,Right,NewCountRemote,0,SetMode);
				[speed,Delay]->
					pcbnif:set_speed(Delay, Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,SetMode);
				[dir,Value]->
					case SetMode of
						10 -> pcbnif:set_direction(4, Pcb_Address);
						_ -> pcbnif:set_direction(Value, Pcb_Address)
					end,
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,SetMode);
				[leftLight,Value]->
					loop(Pcb_Address,ClientPid,State,0,Count,Value,Right,NewCountRemote,0,SetMode);
				[rightLight,Value]->
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Value,NewCountRemote,0,SetMode);
				[brakeLight,Value]->
					pcbnif:set_brakeLight(Value, Pcb_Address),
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,SetMode)
			after
				10 ->
					loop(Pcb_Address,ClientPid,State,0,Count,Left,Right,NewCountRemote,0,SetMode)
			end 
	end.

timeLoop(Pid,State) ->
	timer:sleep(?LEDINTERVAL),
	case State of
		0 -> NewState=1;
		_ -> NewState=0
        end,
	Pid ! [changeState,NewState],
	timeLoop(Pid,NewState).

switch(Pcb_Address,Pid,Sw1,Sw2,Mode,SetMode) ->
        Csw1= pcbnif:get_Switch1(Pcb_Address),
        Csw2= pcbnif:get_Switch2(Pcb_Address),
        case Sw1==1 andalso Csw1==0 of
		true ->
			case Mode==6 of
				true ->
					NewMode=0;
				false ->
					NewMode=Mode+1
			end,
			Pid ! [changeMode,NewMode];
		false->
			NewMode=Mode
        end,
        case Sw2==1 andalso Csw2==0 of
		true ->
			NewSetMode = Mode,
			Pid ! [setMode,NewSetMode];
		false ->
			NewSetMode = SetMode
        end,
        timer:sleep(1),
        switch(Pcb_Address,Pid,Csw1,Csw2,NewMode,NewSetMode).
