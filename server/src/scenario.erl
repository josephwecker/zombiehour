-module(scenario).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

start() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  Self = self(),
  Map = digraph:new(),
  tile:initialize_map(Map),
  Characters = [],
  Vertices = digraph:vertices(Map),
  Zombies = lists:map(
    fun(_Num) ->
        Position = lists:nth(random:uniform(length(Vertices)), Vertices),
        {ok, Pid} = zombie:create([Self, Position, Map]),
        update_map(Map, Position, character, Pid),
        Pid
    end,
    lists:seq(1,30)),
  spawn(fun() -> tick:tick(Self, 100) end),
  {ok, {Characters, Zombies, Map}}.

%Tile functions... (maybe should go in separate module?)
% End of tile functions...

update_map(Map, Vertex, Attr, Value) ->
  {Vertex, Tile} = digraph:vertex(Map, Vertex),
  case Attr of
    character ->
      case Value of
        nil ->
          Tile2 = dict:store(character, nil, Tile),
          NewTile = dict:store(movement, true, Tile2);
        Value ->
          Character = gen_server:call(Value, character),
          Tile2   = dict:store(movement, false, Tile),
          NewTile = dict:store(character, Character, Tile2)
      end;
    Attr ->
      io:format("No update function for: ~p~n",[Attr]),
      NewTile = Tile
  end,
  NewTile2 = tile:update_sym(NewTile),
  digraph:add_vertex(Map, Vertex, NewTile2).

handle_call({create_character, Name}, _From, {Characters, Z, Map}) ->
  Vertices = digraph:vertices(Map),
  Location = lists:nth(random:uniform(length(Vertices)), Vertices),
  {ok, Character} = player:create([Name, self(), Map, Location]),
  update_map(Map, Location, character, Character),
  NewCharacters = lists:append(Characters,[Character]),
  {reply, Character, {NewCharacters, Z, Map}};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Characters, Zombies, _} = State) ->
  lists:foreach(fun(Character) -> gen_server:cast(Character, tick) end, Characters),
  lists:foreach(fun(Zombie) -> gen_server:cast(Zombie, tick) end, Zombies),
  {noreply, State};

handle_cast({wait, Pid}, State) ->
  gen_server:cast(Pid, unlock),
  {noreply, State};

handle_cast({attack, {Attacker, Direction}}, {C, Z, Map}) ->
  Neighbor = nav:neighbor(dict:fetch(location, Attacker), Direction),
  {_, NbrTile} = digraph:vertex(Map, Neighbor),
  Target = dict:fetch(character, NbrTile),
  Pid = dict:fetch(id, Attacker),
  case Target of
    nil ->
      gen_server:cast(Pid, {msg, "You swing at the open air."})
      ;
    Target ->
      TPid = dict:fetch(id,Target),
      case random:uniform(2) of
        1 ->
          gen_server:cast(Pid, {msg, "You miss."}),
          gen_server:cast(TPid, {msg, "The Zombie swings at you but misses."})
          ;
        2 ->
          gen_server:cast(Pid, {msg, "You hit."}),
          gen_server:cast(TPid, {take_damage, 2}),
          gen_server:cast(TPid, {msg, "You take 2 damage"})
      end
  end,
  gen_server:cast(Pid, {heat_up, 8}),
  gen_server:cast(Pid, unlock),
  {noreply, {C, Z, Map}};

handle_cast({walk, {Character, Direction}}, {C, Z, Map}) ->
  Pid = dict:fetch(id, Character),
  case Direction of
    "" ->
      gen_server:cast(Pid, unlock);
    Direction ->
      DesiredLocation = nav:neighbor(dict:fetch(location, Character), Direction),
      case digraph:vertex(Map, DesiredLocation) of
        false ->
          gen_server:cast(Pid, unlock);
        {_, TileData} ->
          case dict:fetch(movement, TileData) of
            false ->
              case dict:fetch(character, TileData) of
                nil ->
                  gen_server:cast(Pid, unlock);
                Target ->
                  case dict:fetch(zombified, Target) == dict:fetch(zombified,
                      Character) of
                      true ->
                        gen_server:cast(Pid, unlock);
                      false ->
                        gen_server:cast(self(), {attack, {Character, Direction}})
                    end
              end;
            true ->
              Location = dict:fetch(location, Character),
              NewLocation = DesiredLocation,
              update_map(Map, Location, character, nil),
              update_map(Map, NewLocation, character, Pid),
              gen_server:cast(Pid, {update_character, {location, NewLocation}}),
              case dict:fetch(zombified, Character) of
                true ->
                  gen_server:cast(Pid, {heat_up, 10});
                false ->
                  gen_server:cast(Pid, {heat_up, 8})
              end,
              gen_server:cast(Pid, unlock)
          end
      end
  end,
  {noreply, {C, Z, Map}};

handle_cast({die, Character}, {C, Z, Map}) ->
  case dict:fetch(zombified, Character) of
    true ->
      NS = {C, lists:delete(dict:fetch(id,Character), Z), Map};
    false ->
      %NS = {lists:delete(dict:fetch(id,Character), C), Z, Map}
      NS = {C,Z,Map}
  end,
  Location = dict:fetch(location, Character),
  update_map(Map, Location, character, nil),
  {noreply, NS};

handle_cast({say, {Character, Msg}}, {Characters, _Z, _} = State) ->
  Name = dict:fetch(tag, Character),
  Output = lists:concat([Name, ": ", Msg]),
  lists:foreach(fun(Char) -> gen_server:cast(Char, {hear, Output}) end, Characters),
  {noreply, State};

handle_cast(Msg, State) ->
  io:format("Scenario received unkown cast: ~p~n",[Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
