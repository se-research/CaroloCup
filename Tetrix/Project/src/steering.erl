-module(steering).
-export([findcircle/3, calculate/5,
        aim/2, followcircle/4, getAng/2, getDistance/2, normalized/2]).

-include("../include/offsetCalculation.hrl").


% create record with radius and clockwise 


calculate({X1,Y1}, {X2,Y2}, {X3,Y3}, CarPos, CarHeading) ->
    case findcircle({X1,Y1}, {X2,Y2}, {X3,Y3}) of 
    	infinate ->
    	    SteerDirection = getAng(CarPos, {X3,Y3});
    	{CenterPoint, Radius, Clockwise}  -> 
	    SteerDirection = followcircle(CarPos, CenterPoint, Radius, Clockwise);
	_ ->
	    SteerDirection = getAng(CarPos, {X3,Y3})
    end,
    normalized(SteerDirection, CarHeading).


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
    
    CenterPointX = (Line1*Line2*(Y3-Y1)+Line1*(X2+X3) - 
    Line2*(X1+X2))/(2*(Line1-Line2)),
    CenterPointY = -(1/Line1)*(CenterPointX -
    ((X1+X2)/2) + (Y1 + Y2) / 2),
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
    CorrectionAng = (1-(LocationOffset/Radius))*(Clockwise*math:pi()/2),
    CenterAng + TangentAngOffset + CorrectionAng.
    
normalized(SteeringDirection, CarHeading) ->
    NewSteer = SteeringDirection rem math:pi() * 2,
    ((NewSteer rem 2*math:pi()) + (2*math:pi()) -
    (CarHeading rem 2*math:pi()) + (2*math:pi())) rem
	2*math:pi().
    
    

	       
getAng({X1,Y1} , {X2,Y2}) -> 
    math:atan(((Y2-Y1)/(X2-X1)) + 
    math:pi()/2 -(math:pi()/2 *
    ((X2-X1)/abs(X2-X1)))).
					
getDistance({X1,Y1} , {X2,Y2}) ->
    math:sqrt(math:pow(Y2-Y1,2) + math:pow(X2-X1,2)).
   
