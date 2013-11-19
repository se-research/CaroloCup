-module(currentPos).
-compile(export_all).

calculatePos(MegaSec, Sec, MiliSec, Speed, RazorData, SteeringRad, SteeringDeg, RazorInit, CarX, CarY, PrevX, PrevY)->

    DeltaTime = (MegaSec*1000000+Sec+MiliSec/1000000)-(MegaSec*1000000+Sec+MiliSec/1000000),
    
    CorrectedSpeed = speedCorrection(Speed),

    DeltaDistance = DeltaTime * CorrectedSpeed * 1000,
    
    CarHeading = RazorData - RazorInit + 90, 

    RazorXPos = PrevX + (DeltaDistance * ( math:cos(CarHeading * math:pi()/180))),
    
    RazorYPos = PrevY + (DeltaDistance * ( math:sin(CarHeading * math:po()/180))).

speedCorrection(Speed) ->
    Speed.
