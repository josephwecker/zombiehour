-module(scenario).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

start() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  Self = self(),
  Characters = [],
  spawn(fun() -> tick:tick(Self, 200) end),
  MapData = [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
             [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
             [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
             [0,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,1,1,1,1,0],
             [0,1,1,1,1,1,1,1,1,1,1,1,1,2,4,4,4,4,4,4,4,2,1,1,1,1,0],
             [0,1,1,2,2,2,2,1,1,1,1,1,1,3,4,4,4,4,4,4,4,2,1,1,1,1,0],
             [0,1,1,2,4,4,2,1,1,1,1,1,1,3,4,4,4,4,4,4,2,2,1,1,1,1,0],
             [0,1,1,2,4,4,3,1,1,1,1,1,1,2,4,4,4,4,4,2,2,1,1,1,1,1,0],
             [0,1,1,2,4,4,2,1,1,1,1,1,1,2,4,4,4,4,4,2,1,1,1,1,1,1,0],
             [0,1,1,2,2,2,2,1,1,1,1,1,1,2,4,4,4,4,4,2,1,1,1,1,1,1,0],
             [0,1,1,1,1,1,1,1,1,1,1,1,1,2,4,4,4,4,4,2,1,1,1,1,1,1,0],
             [0,1,1,1,1,1,1,1,1,1,1,1,1,2,4,4,4,4,4,2,1,1,5,5,1,1,0],
             [0,1,1,1,1,1,1,1,2,2,2,1,1,2,4,4,4,4,4,2,1,1,5,5,1,1,0],
             [0,1,1,1,1,1,2,3,2,4,2,1,1,2,4,4,4,4,4,2,1,1,5,5,1,1,0],
             [0,1,1,1,1,1,2,4,4,4,2,1,1,2,2,2,2,2,2,2,1,1,1,1,1,1,0],
             [0,1,1,1,1,1,2,4,4,4,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
             [0,1,1,1,1,1,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
             [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
             [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],
  Map = digraph:new(),
  initialize_map(Map, MapData),
  {ok, {Characters, Map}}.

%Tile functions... (maybe should go in separate module?)
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
  T = dict:new(),
  T1 = dict:store(x, X, T),
  T2 = dict:store(y, Y, T1),
  T3 = dict:store(characters, [], T2),
  case Value of
    2 ->
      Tile = dict:store(blocking, true, T3);
    _ ->
      Tile = dict:store(blocking, false, T3)
  end,
  %vertex = {x000y000, Tile}
  digraph:add_vertex(Map, Key, Tile).

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
        %edge   = {x000y000direction, []}
        digraph:add_edge(Map, Key, Tile, NTile, [])
    end,
    List).

coords_to_key(XInt,YInt) ->
  X = integer_to_list(XInt),
  Y = integer_to_list(YInt),
  list_to_atom(lists:flatten(["X", X, "Y", Y ])).

dir_to_key(Tile, Dir) ->
  list_to_atom(lists:flatten([atom_to_list(Tile), Dir])).

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
% End of tile functions...

update_map(Map, Vertex, Attr, {Instruction, Value}) ->
  {Vertex, Tile} = digraph:vertex(Map, Vertex),
  case Attr of
    characters ->
      CharList = dict:fetch(characters, Tile),
      case Instruction of
        add ->
          NewList = lists:append(CharList, [Value]);
        rm ->
          NewList = lists:delete(Value, CharList)
      end,
      NewTile = dict:store(characters, NewList, Tile);
    Attr ->
      io:format("No update function for: ~p~n",[Attr]),
      NewTile = Tile
  end,
  digraph:add_vertex(Map, Vertex, NewTile).

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Characters, _} = State) ->
  lists:foreach(fun(Character) -> gen_server:cast(Character, tick) end, Characters),
  {noreply, State};

handle_cast({walk, {Character, Direction}}, {Chars, Map}) ->
  Location = dict:fetch(location, Character),
  Edge = dir_to_key(Location, Direction),
  Result = digraph:edge(Map, Edge),
  {_, _, NewLocation, _} = Result,
  % This is just the updated feedback stuffs...
  {NewLocation, TileData} = digraph:vertex(Map, NewLocation),
  NewX = dict:fetch(x, TileData),
  NewY = dict:fetch(y, TileData),
  Message = lists:concat(["You're now at; ", NewX, ",", NewY, "<br/>"]),
  gen_server:cast(dict:fetch(id, Character), {update_character, {feedback, Message}}),
  % end of feedback stuffs
  Pid = dict:fetch(id, Character),
  gen_server:cast(Pid, {update_character, {location, NewLocation}}),
  {noreply, {Chars, Map}};

handle_cast({say, Msg}, {Characters, _} = State) ->
  lists:foreach(fun(Character) -> gen_server:cast(Character, {hear, Msg}) end, Characters),
  {noreply, State};

handle_cast({add_character, Character}, {Characters, Map}) ->
  Vertices = digraph:vertices(Map),
  Location = lists:nth(random:uniform(length(Vertices)), Vertices),
  update_map(Map, Location, characters, {add, Character}),
  gen_server:cast(Character, {update_character, {location, Location}}),
  NewList = lists:append(Characters,[Character]),
  {noreply, {NewList, Map}};

handle_cast(Msg, State) ->
  io:format("~p~n",[Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
