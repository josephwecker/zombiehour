-module(scenario).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

start() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  Self = self(),
  Map = digraph:new(),
  SpawnPoints = {ZSpawn, _} = tile:initialize_map(Map),
  Characters = [],
  Zombies = lists:map(
    fun(_Num) ->
        spawn_zombie(Map, ZSpawn)
    end,
    lists:seq(1,30)),
  spawn(fun() -> tick:tick(Self, 100) end),
  Board = [],
  StartTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  ZombiesLeft = length(Zombies),
  %CharactersAlive = TotalCharacters = length(Characters),
  Objectives = {StartTime, ZombiesLeft, {0,0}},
  {ok, {{{Characters, Zombies}, Board, SpawnPoints}, Map, Objectives}}.

%Tile functions... (maybe should go in separate module?)
% End of tile functions...
spawn_zombie(Map, SpawnPoints) ->
  Position = lists:nth(random:uniform(length(SpawnPoints)), SpawnPoints),
  {ok, Pid} = zombie:create([self(), Position, Map]),
  update_map(Map, Position, character, Pid),
  Pid.

spawn_player(Map, SpawnPoints, [Name]) ->
  Position = lists:nth(random:uniform(length(SpawnPoints)), SpawnPoints),
  {ok, Pid} = player:create([Name, self(), Map, Position]),
  update_map(Map, Position, character, Pid),
  Pid.

update_map(Character) ->
  Map = dict:fetch(map, Character),
  Vertex = dict:fetch(location, Character),
  {Vertex, Tile} = digraph:vertex(Map, Vertex),
  NewTile = dict:store(character, Character, Tile),
  NewTile2 = tile:update_tile(NewTile),
  digraph:add_vertex(Map, Vertex, NewTile2).

update_map(Map, Vertex, Attr, Value) ->
  {Vertex, Tile} = digraph:vertex(Map, Vertex),
  case Attr of
    character ->
      case Value of
        nil ->
          NewTile = dict:store(character, nil, Tile);
        Value ->
          Character = gen_server:call(Value, character),
          NewTile = dict:store(character, Character, Tile)
      end;
    value ->
      NewTile = dict:store(value, Value, Tile);      
    Attr ->
      io:format("No update function for: ~p~n",[Attr]),
      NewTile = Tile
  end,
  NewTile2 = tile:update_tile(NewTile),
  digraph:add_vertex(Map, Vertex, NewTile2).

check_objectives({{{Characters, Zombies}, _, _}, _, {T,ZL,{CLength,CLivingLength}}} = State) ->
  case ZL < 1 of
    true ->
      {stop, normal, {Characters, Zombies}};
    false ->
      case CLength >= 1 andalso CLivingLength =< 0 of
        true ->
          {stop, normal, {Characters, Zombies}};
        false ->
          TimeNow = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
          case TimeNow - T >= 1800 of
            true ->
              {stop, normal, {Characters, Zombies}};
            false ->
              {noreply, State}
          end
      end
  end.

handle_call({create_character, Name}, _From, {{{Characters, Z},B,{ZP,CP}}, Map,
    {T, ZL, {CLength, CLivingLength}}}) ->
  Character = spawn_player(Map, CP, [Name]),
  NewCharacters = [Character|Characters],
  {reply, Character, {{{NewCharacters, Z},B,{ZP,CP}}, Map, {T, ZL, {CLength + 1,
          CLivingLength + 1}}}};

handle_call(Request, _From, State) ->
  io:format("Scenario received unknown call: ~p when State was: ~p~n~n~n",[Request,
      State]),
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {{{Characters, Zombies}, _, _}, _, _} = State) ->
  lists:foreach(fun(Character) -> gen_server:cast(Character, tick) end, Characters),
  lists:foreach(fun(Zombie) -> gen_server:cast(Zombie, tick) end, Zombies),
  check_objectives(State);

handle_cast({wait, Pid}, State) ->
  gen_server:cast(Pid, unlock),
  {noreply, State};

handle_cast({attack, {Attacker, Direction}}, {C, Map, O}) ->
  Neighbor = nav:neighbor(dict:fetch(location, Attacker), Direction),
  {Neighbor, NbrTile} = digraph:vertex(Map, Neighbor),
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
          gen_server:cast(TPid, {msg, "The Zombie swings at you but misses."});
        2 ->
          Damage = 2,
          gen_server:cast(TPid, {take_damage, Damage}),
          case dict:fetch(hp, Target) =< Damage of
            true ->
              gen_server:cast(Pid, {msg, "You hit and kill your opponent."}),
              gen_server:cast(Pid, get_kill),
              gen_server:cast(TPid, {msg, "You take <span class='dmg'>2 damage</span> and <span class='dmg'>die</span>."});
            false ->
              gen_server:cast(Pid, {msg, "You hit."}),
              gen_server:cast(TPid, {msg, "You take <span class='dmg'>2 damage</span>."})
          end
      end
  end,
  gen_server:cast(Pid, {heat_up, 16}),
  gen_server:cast(Pid, unlock),
  {noreply, {C, Map, O}};

handle_cast({shoot, {Attacker, Direction}}, {C, Map, O}) ->
  Pid = dict:fetch(id, Attacker),
  Tile = character:find_target(Attacker, Direction),
  case dict:fetch(ammo, Attacker) >= 1 of
    true ->
      case Tile of
        false ->
          gen_server:cast(Pid, {msg, "There are no targets in that direction."});
        Tile ->
          {Tile, TargetTile} = digraph:vertex(Map, Tile),
          Target = dict:fetch(character, TargetTile),
          case Target of
            nil ->
              gen_server:cast(Pid, {msg, "Your target evades."})
              ;
            Target ->
              TPid = dict:fetch(id,Target),
              case random:uniform(2) of
                1 ->
                  gen_server:cast(Pid, {msg, "Your shot misses."});
                2 ->
                  Damage = 2,
                  gen_server:cast(TPid, {take_damage, Damage}),
                  case dict:fetch(hp, Target) =< Damage of
                    true ->
                      gen_server:cast(Pid, {msg, "You shoot and kill your opponent."}),
                      gen_server:cast(Pid, get_kill);
                    false ->
                      gen_server:cast(Pid, {msg, "You shot the Zomber."})
                  end
              end
          end,
          gen_server:cast(Pid, {lose_ammo, 1}),
          gen_server:cast(Pid, {heat_up, 16})
      end;
    false ->
      gen_server:cast(Pid, {msg, "You don't have any ammunition left."})
  end,
  gen_server:cast(Pid, unlock),
  {noreply, {C, Map, O}};

handle_cast({walk, {Character, Direction}}, {C, Map, O}) ->
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
                  gen_server:cast(Pid, {heat_up, 20});
                false ->
                  gen_server:cast(Pid, {heat_up, 16})
              end,
              gen_server:cast(Pid, unlock)
          end
      end
  end,
  {noreply, {C, Map, O}};

handle_cast({open, {Character, Direction}}, {C, Map, O}) ->
  Location = dict:fetch(location, Character),
  Target = nav:neighbor(Location, Direction),
  {Target, TileData} = digraph:vertex(Map, Target),
  Pid = dict:fetch(id, Character),
  case dict:fetch(type,TileData) of
    door ->
      update_map(Map, Target, value, opened),
      gen_server:cast(Pid, {update_character, {location, Location}}),
      gen_server:cast(Pid, {heat_up, 8});
    _ ->
      gen_server:cast(Pid, {msg, "There's no door there."})
  end,
  gen_server:cast(Pid, unlock),
  {noreply, {C, Map, O}};

handle_cast({close, {Character, Direction}}, {C, Map, O}) ->
  Location = dict:fetch(location, Character),
  Target = nav:neighbor(Location, Direction),
  Pid = dict:fetch(id, Character),
  {Target, TileData} = digraph:vertex(Map, Target),
  case dict:fetch(type,TileData) of
    door ->
      update_map(Map, Target, value, closed),
      gen_server:cast(Pid, {update_character, {location, Location}}),
      gen_server:cast(Pid, {heat_up, 8});
    _ ->
      gen_server:cast(Pid, {msg, "There's no door there."})
  end,
  gen_server:cast(Pid, unlock),
  {noreply, {C, Map, O}};

handle_cast({die, Character}, {{{C, Z},B,S}, Map, {T, ZL, {CL, CLivingLength}}}) ->
  case dict:fetch(zombified, Character) of
    true ->
      NS = {{{C, lists:delete(dict:fetch(id,Character), Z)},B,S}, Map,
        {T,length(Z), {CL, CLivingLength}}};
    false ->
      %NS = {lists:delete(dict:fetch(id,Character), C), Z, Map, O}
      NS = {{{C, Z},B,S}, Map, {T, ZL, {CL, CLivingLength - 1}}}
  end,
  Location = dict:fetch(location, Character),
  update_map(Map, Location, character, nil),
  {noreply, NS};

handle_cast({update_board, Character}, {{{Chars, Zombs},Board,S}, M, O}) ->
  Board1 = lists:keydelete(dict:fetch(tag, Character),1, Board),
  NewBoard = [
    {dict:fetch(tag, Character),"- ",
      integer_to_list(dict:fetch(kills, Character))," ",
      dict:fetch(living, Character),"<br/>"} | Board1 ],
  CharStats = lists:flatmap(fun(CharStat)-> tuple_to_list(CharStat) end, NewBoard),
  FlatBoard = lists:concat(["Players-<br/>", CharStats, "Zombies- ", length(Zombs)]),
  lists:foreach(
    fun(Pid) ->
        gen_server:cast(Pid, {update_board, FlatBoard})
    end,
    Chars),
  {noreply, {{{Chars, Zombs}, NewBoard, S}, M, O}};

handle_cast({update_self, Character}, State) ->
  update_map(Character),
  {noreply, State};

handle_cast({say, {Character, Msg}}, {{{Characters, _}, _, _}, _, _} = State) ->
  Name = dict:fetch(tag, Character),
  Output = lists:concat([Name, ": ", Msg]),
  lists:foreach(fun(Char) -> gen_server:cast(Char, {hear, Output}) end, Characters),
  {noreply, State};

handle_cast(Msg, State) ->
  io:format("Scenario received unknown cast: ~p when State was: ~p~n",[Msg,
      State]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, {Characters, Zombies}) ->
  lists:foreach(fun(Character) -> gen_server:cast(Character, stop) end, Characters),
  lists:foreach(fun(Zombie) -> gen_server:cast(Zombie, stop) end, Zombies),
  gen_server:cast(zhandler, {close_scenario, self()}),
  timer:sleep(1000),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
