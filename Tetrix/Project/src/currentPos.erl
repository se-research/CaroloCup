-module(currentPos).
-compile(export_all).

calculatePos(PrevHal, CurrHal, PrevHeading, CurrHeading)->

    NewHal = (CurrHal - PrevHal)*100*(1000/57),

    case (NewHal < 0) of
	true ->
	    DeltaDistance = NewHal + 1000;
	false ->
	    DeltaDistance = NewHal
    end,

    Angle = CurrHeading - PrevHeading,

    DeltaHeading = normalized((Angle*math:pi()/180))*(180/math:pi()),

    PosX = DeltaDistance * ( math:cos( (CurrHeading * math:pi()/180) )),
    
    PosY = DeltaDistance * ( math:sin( (CurrHeading * math:pi()/180) )),
    
    {PosX, PosY, DeltaHeading}.

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
