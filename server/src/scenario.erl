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
  tile:initialize_map(Map, MapData),
  {ok, {Characters, Map}}.

%Tile functions... (maybe should go in separate module?)
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
  Edge = tile:dir_to_key(Location, Direction),
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
  gen_server:cast(Character, {add_location, Location}),
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