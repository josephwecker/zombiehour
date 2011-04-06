-module(tile).
-export([initialize_map/2, coords_to_key/2, dir_to_key/2, update_sym/1]).

initialize_map(Map, MapData) ->
  lists:foreach(
    fun(RowCount) ->
      lists:foreach(
        fun(ColCount) ->
            initialize_tile(Map, MapData, RowCount, ColCount)
        end,
        lists:seq(1,length(lists:nth(RowCount, MapData))))
    end,
    lists:seq(1,length(MapData))),
  lists:foreach(
    fun(Tile) ->
        initialize_neighbors( Tile, Map, length(lists:nth(1,MapData)), length(MapData) )
    end,
    digraph:vertices(Map)).

initialize_tile(Map, MapData, RowCount, ColCount) ->
  X = ColCount,
  Y = RowCount,
  Key = coords_to_key( X, Y ),
  Value = lists:nth(X, lists:nth(Y, MapData)),
  Attrs = [{x, X}, {y, Y}, {characters, []}, {zombies, []}],
  case Value of
    0 ->
      Attrs2 = [{type, map_boundary}, {movement, false}, {visible, false}];
    2 ->
      Attrs2 = [{type, wall}, {movement, false}, {visible, false}];
    3 ->
      Attrs2 = [{type, door}, {movement, true}, {visible, false}];
    5 ->
      Attrs2 = [{type, obstacle}, {movement, false}, {visible, true}];
    6 ->
      Attrs2 = [{type, window}, {movement, false}, {visible, true}];
    _ ->
      Attrs2 = [{type, space}, {movement, true}, {visible, true}]
  end,
  Tile = dict:from_list(lists:append(Attrs, Attrs2)),
  Tile2 = update_sym(Tile),
  %vertex = {"x000y000", Tile}
  digraph:add_vertex(Map, Key, Tile2).

initialize_neighbors( Tile, Map, XBound, YBound ) ->
  {Tile, TileData} = digraph:vertex(Map, Tile),
  X = dict:fetch(x, TileData),
  Y = dict:fetch(y, TileData),
  {X1, X, X2} = find_bound(X, XBound),
  {Y1, Y, Y2} = find_bound(Y, YBound),
  List = [
    {"northwest", coords_to_key( X1, Y2 )},
    {"north"    , coords_to_key( X , Y2 )},
    {"northeast", coords_to_key( X2, Y2 )},
    {     "east", coords_to_key( X2, Y  )},
    {"southeast", coords_to_key( X2, Y1 )},
    {"south"    , coords_to_key( X , Y1 )},
    {"southwest", coords_to_key( X1, Y1 )},
    {     "west", coords_to_key( X1, Y  )}],
  lists:foreach(
    fun(ListItem) ->
        {Direction, NTile} = ListItem,
        Key = dir_to_key(Tile, Direction),
        %edge   = {"x000y000direction", []}
        digraph:add_edge(Map, Key, Tile, NTile, [])
    end,
    List).

coords_to_key(XInt,YInt) ->
  X = integer_to_list(XInt),
  Y = integer_to_list(YInt),
  lists:concat(["X", X, "Y", Y ]).

dir_to_key(Tile, Dir) ->
  lists:concat([Tile, Dir]).

find_bound(D, Limit) ->
  case D of
    1 ->
      D1 = D,
      D2 = D + 1;
    Limit ->
      D1 = D - 1,
      D2 = D;
    D ->
      D1 = D - 1,
      D2 = D + 1
  end,
  {D1, D, D2}.


update_sym(Tile) ->
  case dict:fetch(zombies, Tile) of
    []->
      case dict:fetch(characters, Tile) of
        []->
          case dict:fetch(type, Tile) of
            map_boundary ->
              Symbol = "2";
            space ->
              Symbol = "1";
            wall ->
              Symbol = "2";
            door ->
              Symbol = "3";
            window ->
              Symbol = "4";
            obstacle ->
              Symbol = "5"
          end;
        _ ->
          Symbol = "8"
      end;
    _ ->
      Symbol = "7"
  end,
  dict:store(symbol, Symbol, Tile).
