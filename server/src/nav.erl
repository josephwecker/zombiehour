-module(nav).
-export([position/1, distance/2, direction/2, neighbor/2]).

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

neighbor(Origin, Direction) ->
  {OX, OY} = position(Origin),
  if
    Direction == "north" ->
      Neighbor = tile:coords_to_key(OX, OY+1);
    Direction == "northeast" ->
      Neighbor = tile:coords_to_key(OX+1, OY+1);
    Direction == "east" ->
      Neighbor = tile:coords_to_key(OX+1, OY);
    Direction == "southeast" ->
      Neighbor = tile:coords_to_key(OX+1, OY-1);
    Direction == "south" ->
      Neighbor = tile:coords_to_key(OX, OY-1);
    Direction == "southwest" ->
      Neighbor = tile:coords_to_key(OX-1, OY-1);
    Direction == "west" ->
      Neighbor = tile:coords_to_key(OX-1, OY);
    Direction == "northwest" ->
      Neighbor = tile:coords_to_key(OX-1, OY+1);
    Direction == "" ->
      Neighbor = tile:coords_to_key(OX, OY)
  end,
  Neighbor.
    


%get_cone(Origin, Direction) ->
%  Directions = ["northwest", "north", "northeast", "east",
%    "southeast", "south", "southwest", "west"],
