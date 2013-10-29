-module(steering)
-export([findcircle/5, start/0, init/0, calculate/5, findcircle/5,
        aim/3, followcircle/4, getAng/2, getDistance/2]).

-include("../include/offsetCalculation.hrl").

% create record with radius and clockwise 

-record(steerCalc, {cp, radius, clockwise, circulate, steeringAng, carPos,
        carAng, listPos, overtaking_status, lap_complete, wayList, obstacleNextTo}).


start() ->
    ok.

init() ->
    #steerCalc, {cp = {0,0}, radius = 0, clockwise = 0, circulate = 0,
        steeringAng = 0, carPos = {0,0}, carAng = math:pi()/2 , listPos = 0, 
        overtaking_status = 0, lap_complete = 0, wayList = ?LaneAdjacent,
        obstacleNextTo = 0}).


calculate({X1,Y1}, {X2,Y2}, {X3,Y3}, CarPos, CarHeading) ->
    case findcircle({X1,Y1}, {X2,Y2}, {X3,Y3}, CarPos, CarHeading) of 
    	infinate ->
    	    aim(CarPos, CarHeading, {X3,Y3});
    	{CenterPoint, Radius}  -> 
    	    followcircle(CarPos, CarHeading, CenterPoint, Radius);
	    _ ->
	        aim(CarPos, CarHeading, LastPoint)
    end.


findcircle({X1,Y1}, {X2,Y2}, {X3,Y3}, CarPos, CarHeading) -> 
	   %{X1,Y1} -> {double,double},
	   %{X2,Y2} -> {double,double},
           %{X3,Y3} -> {double,double},
	   %CarPos = node() -> {X,Y} -> {double,double},
	   %CarHeading = node() -> {X,Y} -> {double,double},
	   %Radius = double,
           %CenterPoint = node() -> {X,Y} -> {double,double},
	   %Find = {CenterPoint = node() -> {X,Y} , Radius = double},
           %infinate = {CarPos,CarHeading,P3},
        
    Line1 = ((Y1 - Y2) / (X1 - X2)),
    Line2 = ((Y2 - Y3) / (X2 - X3)),
    
    CenterPointX = (Line1*Line2*(Y3-Y1)+Line1*(X2+X3) - 
    Line2*(X1+X2))/(2*(Line1-Line2)),
    CenterPointY = -(1/Line1)*(CenterPointX -
    ((X1+X2)/2) + (Y1 + Y2) / 2),
    CenterPoint = {CenterPointX,CenterPointY},
    #steerCalc{cp = CenterPoint}, 
    Radius = getDistance(CenterPoint, {X1,Y1}),
    #steerCalc{radius = Radius},

	Line3 = getAng(X1 , Y1 , X2 , Y2),
    case Line3 > 100000 of 
		true -> 
		    infinite;
		false ->
		    Line4 = getAng(X2 , Y2 , X3 , Y3),	
		    case Line4 > 100000 of
				true ->
				    infinite;
				false ->
				    Area = math:cos(Line4+(math:pi()/2-Line3)),
				    {CenterPoint , Radius},
                    Clockwise = Area/(abs(Area)),
                    Circulate = 1,
                    #steerCalc{clockwise = Clockwise, circulate = Circulate}
                    
		     end.	  
           %Clockwise = Area/(abs(Area)),
           %Circulate = 1.
					    

aim(CarPos, CarHeading, LastPoint) ->
    
    %Aim = {SteeringAng = double}.
    SteeringAng = 
    ok.

followcircle(CarPos, CarHeading, CenterPoint, Radius, ClockWise) -> 
	       %Aim = {SteeringAng = double}.
    %CenterAng = getAng(CarPos , CenterPoint),
    %TangentAngOffset = Clockwise*math:pi()/2,
    %LocationOffset = getDistance(CenterPoint , CarPos), 
    %CorrectionAng = (1-(LocationOffset/Radius))*(Clockwise*math:pi()/2),
    %{CenterAng + TangentAngOffset + CorrectionAng}.
    ok.
    

	       
getAng({X1,Y1} , {X2,Y2}) -> 
    math:atan(((Y2-Y1)/(X2-X1)) + 
    math:pi()/2 -(math:pi()/2 *
    ((X2-X1)/abs(X2-X1)))).
					
getDistance({X1,Y1} , {X2,Y2}) ->
    math:sqrt(math:pow(Y2-Y1,2) + math:pow(X2-X1,2)).
   
