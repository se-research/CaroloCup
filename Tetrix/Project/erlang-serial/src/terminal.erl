%% Copyright (c) 1996, 1999 Johan Bevemyr
%% Copyright (c) 2007, 2009 Tony Garnock-Jones
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%    -*- Erlang -*- 
%    File:	terminal.erl  (~jb/serialport/terminal.erl)
%    Author:	Johan Bevemyr
%    Created:	Wed Oct 23 14:02:13 1996
%    Purpose:   
 
-module(terminal).

-export([start/0, gs_start/1, gs_init/1]).
-export([tty_listner/1]).

-compile(export_all).

-define(DEVICE, "/dev/ttyUSB0").

-define(HAL, ?MODULE).

start() ->
    Speed = 9600,
    Pid = whereis(currentPos),
    X = spawn(?HAL, init, [Pid]),
%    SerialPort = serial:start([{speed,Speed}]), % roland
    SerialPort = serial:start([{speed,Speed},{open,?DEVICE}], X),
    %spawn_link(terminal,tty_listner,[SerialPort]),
    {ok,X}.

init(State) ->
    serial_listner(State,[]).

serial_listner(Pid, State) ->
    receive
	{data, Bytes} ->
	    io:format("BYTE RECEIVED : ~p ~n", [Bytes]),
	    Buff = binary_to_list(Bytes),
	    NewState = parse(Buff,State,Pid),
	    serial_listner(Pid,NewState)
    end.

parse([$| | T], State,Pid) ->
    Pid ! {hal, State},
    parse(T,[],Pid);
parse([$% | T] ,State, Pid) ->
    parse(T,State,Pid);
parse([H|T],State,Pid) ->
    parse(T,State ++ [H],Pid);
parse([],State,Pid) ->
    State.


checker([], Buff, Status) ->
    Buff;

checker([H|T], Buff, true) ->
    case H of
	$| ->
	    true;
	_ ->
	    checker(T, Buff, true)
    end;

checker([H|T], Buff, false) ->
    io:format("Head: ~p ~n", [H]),
    case H of
	$% ->
	    checker(Buff, Buff, true);
	_ ->
	    checker(T, Buff, false)
    end.


tty_listner(SerialPort)  ->
    Char = io:get_line('Terminal> '),
    NewChar = replace(Char,10,13),
    SerialPort ! {send, NewChar},
    tty_listner(SerialPort).

replace([],_X,_Y) -> [];
replace([H|T],H,Y) ->
    [Y|replace(T,H,Y)];
replace([H|T],X,Y) ->
    [H|replace(T,X,Y)].

remove_ctrl([],Buff) -> Buff;
remove_ctrl([H|T], Buff) ->
    case H of
	X when X == 10 ->
	    remove_ctrl(T, Buff ++ [10,13]);
	X when X < 32 ->
	    remove_ctrl(T, Buff);
	X when X > 200 ->
	    remove_ctrl(T, Buff);
	_ ->
	    remove_ctrl(T, Buff++[H])
    end.

gs_remove_ctrl([]) -> [];
gs_remove_ctrl([H|T]) ->
    case H of
	X when X == 10 ->
	    [13 | gs_remove_ctrl(T)];
	X when X < 32 ->
	    gs_remove_ctrl(T);
	X when X > 200 ->
	    gs_remove_ctrl(T);
	_ ->
	    [H | gs_remove_ctrl(T)]
    end.

remove_letters(List,Pid) -> remove_letters(List,[],Pid).
remove_letters([H|T],Buff,Pid) ->
     case H of
	X when X == 37 ->
	    remove_letters(T,Buff,Pid);
        X when X == 124 ->
	     Buff;
%%	    remove_letters(T,Buff,Pid);
	_ ->
	     remove_letters(T,Buff++[H],Pid)
    end;
remove_letters([],Buff,Pid)-> Buff.



	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% GS interface
%

gs_start(Speed) -> spawn(terminal, gs_init, [Speed]).

gs_init(Speed) ->
    I=gs:start(),
    Win=gs:create(window, I,
                  [{width, 500},{height, 400},
                   {title,"terminal"},{map, true},{keypress,true}]),
    gs:create(editor, editor, Win,
              [{x,0},{y, 30},{width,480},{height,350},
               {enable,false},{vscroll,right},{wrap,char}]),
    Bar = gs:create(menubar,Win,[]),
    Fmb = gs:create(menubutton,Bar,[{label,{text,"File"}}]),
    Fmnu= gs:create(menu,Fmb,[]),
    gs:create(menuitem,exit,Fmnu,[{label,{text,"Exit"}}]),
    Smb = gs:create(menubutton,Bar,[{label,{text,"Settings"}}]),
    Smnu= gs:create(menu, Smb, []),
    Spd = gs:create(menuitem,Smnu,[{label,{text,"Speed"}},{itemtype,cascade}]),
    Spmnu=gs:create(menu,Spd,[]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"50"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"75"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"110"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"134"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"150"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"200"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"300"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"600"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"1200"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"1800"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"2400"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"4800"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"9600"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"19200"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"38400"}}]),
    gs:create(menuitem,speed,Spmnu,[{label,{text,"57600"}}]),
    SerialPort = serial:start([{speed,Speed}]), % roland
%   SerialPort = serial:start([{speed,Speed},{open,?DEVICE}]),
    gs:create(menuitem,break,Smnu,[{label,{text,"Send break"}}]),
    gs:create(menuitem,hangup,Smnu,[{label,{text,"Hang up"}}]),
    gs:create(menuitem,disconnect,Smnu,[{label,{text,"Disconnect"}}]),
    gs:create(menuitem,connect,Smnu,[{label,{text,"Connect"}}]),
    gs:create(menuitem,open,Smnu,[{label,{text,"Open "++?DEVICE}}]),
    gs_loop(SerialPort).

gs_loop(Serial) ->
    receive
	{data, Bytes} ->
	    TextStr = gs_remove_ctrl(binary_to_list(Bytes)),
	    gs:config(editor,[{enable, true}]),
            gs:config(editor,[{insert, {insert, TextStr}}]),
	    gs:config(editor,[{enable, false}]),
%	    gs:config(editor,[{enable, false}, {insert, {insert, TextStr}},
%			      {enable, true}])
	    TextSize = gs:read(editor,size),
    	    gs:config(editor,[{vscrollpos,TextSize}]);
	
	{gs,_ObjectId,keypress,_Data,[Keysym,KeyCode,_Shift,Control]} ->
	    case KeyCode of
		X when X > 32, X < 97 ->
		    case Control of
			0 -> 
			    Serial ! {send, [KeyCode]};
			1 ->
			    Serial ! {send, [KeyCode-65]}
		    end;
		X when X < 200 ->
		    Serial ! {send, [KeyCode]};
		_X ->
		    case Keysym of
			'Return' ->
			    Serial ! {send, [13]};
			OtherKeysym ->
			    io:format("OtherKeysym:~w~n", [OtherKeysym])
		    end
	    end;
	{gs,speed,click,_Data,[NewSpeed,_Nr]} ->
	    Serial ! {speed,list_to_integer(NewSpeed)};
	{gs,break,click,_Data,_Opts} ->
	    Serial ! {break};
	{gs,hangup,click,_Data,_Opts} ->
	    Serial ! {disconnect},
	    Serial ! {connect};
	{gs,disconnect,click,_Data,_Opts} ->
	    Serial ! {disconnect};
	{gs,connect,click,_Data,_Opts} ->
	    Serial ! {connect};
	{gs,open,click,_Data,_Opts} ->
	    Serial ! {open,?DEVICE};
	{gs,exit,click,_Data,_Args} ->
	    Serial ! stop,
	    exit(normal);
	{gs,_ObjectId,destroy,[],[]} ->
	    Serial ! stop,
	    exit(normal);
        Other ->
            io:format("Other:~w~n",[Other])
    end,
    gs_loop(Serial).

