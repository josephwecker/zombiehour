-module(nav).
-export([position/1, distance/2, direction/2]).

position(Location) ->
  [XStr, YStr] = string:tokens(Location, "XY"),
  X = list_to_integer(XStr),
  Y = list_to_integer(YStr),
  {X, Y}.

distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow((X1 - X2), 2) + math:pow((Y1 - Y2), 2));

distance(Location1, Location2) ->
  Pos1 = position(Location1),
  Pos2 = position(Location2),
  distance(Pos1, Pos2).

direction({X1, Y1}, {X2, Y2}) ->
  if
    Y1 == Y2 ->
      NS = "";
    Y1 < Y2 ->
      NS = "north";
    Y1 > Y2 ->
      NS = "south"
  end,
  if
    X1 == X2 ->
      EW = "";
    X1 < X2 ->
      EW = "east";
    X1 > X2 ->
      EW = "west"
  end,
  lists:concat([NS, EW]);
  
direction(Location1, Location2) ->
  Pos1 = position(Location1),
  Pos2 = position(Location2),
  direction(Pos1, Pos2).

