-module(steering)
-export([findcircle/5, start/0, init/0, calculate/5, findcircle/5
        aim/3, followcircle/4, getAng/2, getDistance/2]).


start() ->
    

init() ->
    

calculate({X1,Y1}, {X2,Y2}, {X3,Y3}, CarPos, CarHeading) ->
    case findcircle({X1,Y1}, {X2,Y2}, {X3,Y3}, CarPos, CarHeading) of 
	infinate ->
	    aim(CarPos, CarHeading, LastPoint);
	{CenterPoint, Radius}  -> 
	    followcircle(CarPos, CarHeading, CenterPoint, Radius);
	_ ->
	    aim(CarPos, CarHeading, LastPoint)
    end.


findcircle(P1, P2, P3, CarPos, CarHeading) -> Find
	   P1 = node() -> {X1,Y1} -> {double,double}
	   P2 = node() -> {X2,Y2} -> {double,double}
           P3 = node() -> {X3,Y3} -> {double,double}
	   CarPos = node() -> {X,Y} -> {double,double}
	   CarHeading = node() -> {X,Y} -> {double,double}
	   Radius = double
           CenterPoint = node() -> {X,Y} -> {double,double}
	   Find = {CenterPoint = node() -> {X,Y} , Radius = double}.
           infinate = {CarPos,CarHeading,P3}


aim(CarPos, CarHeading, LastPoint) -> Aim				  
	       Aim = {SteeringAng = double}.


followcircle(CarPos, CarHeading, CenterPoint, Radius) -> Aim
	       Aim = {SteeringAng = double}.
	       
getAng({X1,Y1} , {X2,Y2}) -> 
    math:atan(((Y2-Y1)/(X2-X1)) + 
    math:pi()/2 -(math:pi()/2 *
    ((X2-X1)/abs(X2-X1)))).
					
			
		       
getDistance({X1,Y1} , {X2,Y2}) ->
    math:sqrt(math:pow(Y2-Y1,2) + math:pow(X2-X1,2)).


    
					    
					  
			       
			       
			       
	   
						   
	   
		   
    

    
    
