-module(los).
-export([character_los/1]).

character_los( Character ) ->
  Map = ets:lookup_element(Character, map, 2),
  Origin = ets:lookup_element(Character, location, 2),
  Radius = ets:lookup_element(Character, sight, 2),
  los( Map, Origin, Radius ).

los( Map, Origin, Radius ) ->
  Quarter = trunc(Radius/2.0) + 2,
  RadiusSquared = math:pow(Radius,2),
  [XStr, YStr] = string:tokens(Origin, "XY"),
  OrigX = list_to_integer(XStr),
  OrigY = list_to_integer(YStr), 
  Quadrants = [{-1,1},{1,1},{1,-1},{-1,-1}],
  lists:umerge(
    lists:map(
      fun({QX, QY}) ->
          List = lists:umerge(
            lists:map(
              fun(X) ->
                  NewX = QX * X,
                  NewY = QY * trunc(math:sqrt( RadiusSquared - math:pow(X,2) )),
                  lists:umerge( get_line( Map, {OrigX, OrigY}, {NewY, NewX} ),
                    get_line( Map, {OrigX, OrigY}, {NewX, NewY} ))
              end,
              lists:seq(0, Quarter))),
          Diag = trunc(math:sqrt( RadiusSquared / 2 )),
          lists:umerge( List, get_line( Map, {OrigX, OrigY}, {Diag * QX, Diag * QY} ) )
      end,
      Quadrants)).


get_line(Map, {X, Y}, {XOffset, YOffset}) ->
  line(Map, {X, Y}, {X + XOffset, Y + YOffset}).

line(Map, {X0, Y0}, {X1, Y1}) ->
	SX = step(X0, X1),
	SY = step(Y0, Y1),
	DX = abs(X1 - X0),
	DY = abs(Y1 - Y0),
	Err = DX - DY,
	line(Map, {X0, Y0}, {X1, Y1}, {SX, SY}, {DX, DY}, Err, []).
 
line(_Map, {X1, Y1}, {X1, Y1}, _, _, _, Tiles) ->
  Key = tile:coords_to_key(X1, Y1),
  end_line([Key | Tiles]);

line(Map, {X, Y}, {X1, Y1}, {SX, SY}, {DX, DY}, Err, Tiles) ->
  Key = tile:coords_to_key(X, Y),
  {Key, Tile} = digraph:vertex(Map, Key),
  case dict:fetch(visible, Tile) of
    false ->
      end_line([Key | Tiles]);
    true ->
      DE = 2 * Err,
      {X0, Err0} = next_x(X, SX, DY, Err, DE),
      {Y0, Err1} = next_y(Y, SY, DX, Err0, DE),
      line(Map, {X0, Y0}, {X1, Y1}, {SX, SY}, {DX, DY}, Err1, [Key | Tiles])
  end.

end_line(Tiles) ->
  lists:sort(Tiles).

step(P0, P1) when P0 < P1 ->
	1;
step(_, _) ->
	-1.
 
next_x(X, SX, DY, E, DE) when DE > -DY ->
	{X + SX, E - DY};
next_x(X, _SX, _DY, E, _DE) ->
	{X, E}.

next_y(Y, SY, DX, E, DE) when DE < DX ->
	{Y + SY, E + DX};
next_y(Y, _SY, _DX, E, _DE) ->
	{Y, E}.
