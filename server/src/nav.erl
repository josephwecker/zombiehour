-module(nav).
-export([position/1, distance/2, direction/2, neighbor/2, get_quadrant/3]).

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
      NS = "south"%
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
    
get_quadrant(Origin, "", _Length) -> [Origin];

get_quadrant(Origin, Direction, Length) ->
  %Directions = ["northwest", "north", "northeast", "east",
  %  "southeast", "south", "southwest", "west"],
  %A = Direction,
  %B = Directions[Direction -1 ],
  %C = Directions[Direction +1 ],

  case Direction of
    "north" ->
      A = "north",
      B = "northwest",
      C = "northeast";
    "northeast" ->
      A = "northeast",
      B = "north",
      C = "east";
    "east" ->
      A = "east",
      B = "northeast",
      C = "southeast";
    "southeast" ->
      A = "southeast",
      B = "east",
      C = "south";
    "south" ->
      A = "south",
      B = "southeast",
      C = "southwest";
    "southwest" ->
      A = "southwest",
      B = "south",
      C = "west";
    "west" ->
      A = "west",
      B = "southwest",
      C = "northwest";
    "northwest" ->
      A = "northwest",
      B = "west",
      C = "north"
  end,

  Start = neighbor(Origin, A),
  Left = neighbor(Start, B),
  Right = neighbor(Start, C),
  LatestTiles = [neighbor(Start, A), Left, Right],
  lists:append([Start|LatestTiles],
  add_to_quadrant(A, B, C, LatestTiles, Left, Right, Length -2)).

add_to_quadrant(A, B, C, LatestTiles, Left, Right, 1) ->
  NewLeft = neighbor(Left, B),
  NewRight = neighbor(Right, C),
  lists:append(
    lists:map(
      fun(Tile) ->
          neighbor(Tile, A)
      end,
      LatestTiles),
    [NewLeft, NewRight]);

add_to_quadrant(A, B, C, LatestTiles, Left, Right, Length) ->
  NewLeft = neighbor(Left, B),
  NewRight = neighbor(Right, C),
  NewLatestTiles = lists:append(
    lists:map(
      fun(Tile) ->
          neighbor(Tile, A)
      end,
      LatestTiles),
    [NewLeft, NewRight]),
  lists:append(NewLatestTiles,
    add_to_quadrant(A, B, C, NewLatestTiles, NewLeft, NewRight, Length -1)).
