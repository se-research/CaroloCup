% @doc
% Provides the macro definitions used by offsetCalculation.erl

-define(PI, 3.141599265359).
-define(AdjacentSideLine, 0).
-define(LaneAdjacent, 1).
-define(DashLine, 2).
-define(LaneOpposite, 3).
-define(OppositeSideLine, 4).
-define(InputLaneL, -1).
-define(InputLaneR, 1).
-define(InputLaneD, 0).
-define(RoadThickness, 80).
-define(OffsetLaneThickness, ?RoadThickness/2).
-define(OffsetLaneAdjacent, ?OffsetLaneThickness/2).
-define(OffsetLaneOpposite, ?OffsetLaneThickness + ?OffsetLaneAdjacent).
-define(AdjustantLaneOpposite, 0).
