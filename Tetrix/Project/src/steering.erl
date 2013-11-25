-module(steering).
-export([findcircle/3, calculate/5,
        aim/2, followcircle/4, getAng/2,
	getDistance/2, normalized/1,
        local_to_global/3]).

-include("../include/offsetCalculation.hrl").
-define(frem(A,B), A - (trunc(A/B) * B)). %%trunc(A) rem B + A - trunc)

% create record with radius and clockwise 


calculate({X1,Y1}, {X2,Y2}, {X3,Y3}, CarPos, CarHeading) ->
    %%    io:format("~p,~p,~p,~p,~p" , [ {X1,Y1}, {X2,Y2}, {X3,Y3} , CarPos, CarHeading]),
    ValX = min(abs(X1-X2)*1000000, abs(X2-X3)*1000000),
    ValY = min(abs(Y1-Y2)*1000000, abs(Y2-Y3)*1000000),
    case  {ValX < 1 , ValY < 1} of 
	{false, false} ->
	    %% io:format("HERE 1~n" , []),
	    case findcircle({X1,Y1}, {X2,Y2}, {X3,Y3}) of 
		infinate ->
		    io:format("HERE 2~n" , []),
		    SteerDirection = getAng(CarPos, {X3,Y3});
		{CenterPoint, Radius, Clockwise}  -> 
		    %% io:format("CP ~p, R ~p , CL ~p ~n" , [CenterPoint, Radius, Clockwise]),
		    SteerDirection = followcircle(CarPos, CenterPoint, Radius, Clockwise);
		_ ->
		    io:format("HERE 3~n" , []),
		    SteerDirection = getAng(CarPos, {X3,Y3})	
	    end;
	_ ->
	    SteerDirection = getAng(CarPos, {X3,Y3})
    end,
    Rem_of_Steer = ?frem(SteerDirection, (math:pi() * 2.0) ) + (math:pi() * 2) ,
    Rem_of_Head = ?frem(CarHeading, (math:pi() * 2.0) ) + (math:pi() * 2),
    Final_Steer = ?frem( (Rem_of_Steer - Rem_of_Head) , (math:pi() * 2.0) ), 
    normalized(Final_Steer).


findcircle({X1,Y1}, {X2,Y2}, {X3,Y3}) -> 
    %% {X1,Y1} -> {double,double},
    %% {X2,Y2} -> {double,double},
    %% {X3,Y3} -> {double,double},
    %% CarPos = node() -> {X,Y} -> {double,double},
    %% CarHeading = node() -> {X,Y} -> {double,double},
    %% Radius = double,
    %% CenterPoint = node() -> {X,Y} -> {double,double},
    %% Find = {CenterPoint = node() -> {X,Y} , Radius = double},
    %% infinate = {CarPos,CarHeading,P3},
    
    Line1 = ((Y1 - Y2) / (X1 - X2)),
   

    Line2 = ((Y2 - Y3) / (X2 - X3)),
    
    CenterPointX = (Line1*Line2*(Y3-Y1)+ Line1*(X2+X3) - Line2*(X1+X2)) / (2*(Line1-Line2)),
    CenterPointY = -(1/Line1) * (CenterPointX - ((X1+X2)/2)) + (Y1 + Y2) / 2,
    CenterPoint = {CenterPointX,CenterPointY},
    %% #steerCalc{cp = CenterPoint}, 
    Radius = getDistance(CenterPoint, {X1,Y1}),
    %% #steerCalc{radius = Radius},

    Line3 = getAng({X1 , Y1} , {X2 , Y2}),
    case Line3 > 100000 of 
	true -> 
	    infinite;
	false ->
	    Line4 = getAng({X2 , Y2} , {X3 , Y3}),	
	    case Line4 > 100000 of
		true ->
		    infinite;
		false ->
		    Area = math:cos(Line4+(math:pi()/2-Line3)),
		    Clockwise = Area/(abs(Area)),		 
		    {CenterPoint , Radius , Clockwise}
	    end
    end.	  


aim(CarPos, LastPoint) ->
    %% Aim = {SteeringAng = double}.
    getAng(CarPos,LastPoint).


followcircle(CarPos, CenterPoint, Radius, Clockwise) -> 
    %% Aim = {SteeringAng = double}.
    CenterAng = getAng(CarPos , CenterPoint),
    TangentAngOffset = Clockwise*math:pi()/2,
    LocationOffset = getDistance(CenterPoint , CarPos), 
    CorrectionAng = (1-(LocationOffset/(Radius)))*(Clockwise*math:pi()/2),
    CenterAng + TangentAngOffset + CorrectionAng.
    
normalized(Angle)->
    case {Angle > math:pi() , Angle < -math:pi()} of
	{true, _} ->
	    NewAngle = normalize(Angle, (-2.0 * math:pi()) );
	{_,true} ->
	    NewAngle = normalize(Angle, (2.0 * math:pi()) );
	_ ->
	    NewAngle = Angle
    end,
    case abs(NewAngle) == math:pi() of
	true ->
	    0.0;
	_ ->
	    NewAngle
    end.

normalize(Angle, MyPI) ->
    case {Angle > math:pi() , Angle < -math:pi()} of
	{false,false} ->
	    Angle;
	_ ->
	    normalize(Angle+MyPI, MyPI)
    end.
	       
getAng({X1,Y1} , {X2,Y2}) -> 
    math:atan2(Y2-Y1,X2-X1).
					
getDistance({X1,Y1} , {X2,Y2}) ->
    math:sqrt(math:pow(Y2-Y1,2) + math:pow(X2-X1,2)).

local_to_global({CarX, CarY}, CarAng, {CoordXRaw, CoordYRaw}) ->
    CoordXin = CoordXRaw + 4,
    CoordYin = CoordYRaw + (-67),
    CoordXZero = round(CoordXin * 100000) == 0,
    CoordYZero = round(CoordYin * 100000) == 0,
    case {CoordXZero,CoordYZero} of 
	{true,true}->
	    CoordX=CoordXin+0.000001,
	    CoordY=CoordYin+0.000001;
	{true,_} ->
	    CoordX=CoordXin+0.000001,
	    CoordY = CoordYin;

	{_,true} ->
	    CoordY=CoordYin+0.000001,
	    CoordX=CoordXin;
	{_,_} -> 
	    CoordX=CoordXin,
	    CoordY=CoordYin
    end,
    X = CarX + (getDistance({0,0},{CoordX,CoordY})*(math:cos(CarAng+(getAng({0,0},{CoordX,CoordY}))))),
    Y = CarY + (getDistance({0,0},{CoordX,CoordY})*(math:sin(CarAng+(getAng({0,0},{CoordX,CoordY}))))),
    {X,Y}.
   
