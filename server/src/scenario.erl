-module(scenario).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, update_map/2, update_map/5]).

start() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  Self = self(),
  Map = digraph:new(),
  SpawnPoints = {_, ZSpawn} = tile:initialize_map(Map),
  Players = ets:new(player_list, [set, public]),
  Zombies = ets:new(zombie_list, [set, public]),
  TickList = [],
  lists:foreach(
    fun(_Num) ->
        spawn_character(zombie, Map, {Players, Zombies}, ZSpawn, [])
    end,
    lists:seq(1,20)),
  spawn(fun() -> tick:tick(Self, 100) end),
  Board = [],
  StartTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  ZombiesLeft = ets:info(Zombies, size),
  %CharactersAlive = TotalCharacters = length(Characters),
  Objectives = {StartTime, ZombiesLeft, {0,0}},
  {ok, {{Players, Zombies, TickList}, Map, SpawnPoints, {Objectives, Board}}}.

spawn_character(Type, Map, Characters, SpawnPoints, Attrs) ->
  character:create([self(), Map, Characters, SpawnPoints, {Type, Attrs}]).

update_map(Character, Scenario) ->
  Map = ets:lookup_element(Character, map, 2),
  Vertex = ets:lookup_element(Character, location, 2),
  {Vertex, Tile} = digraph:vertex(Map, Vertex),
  NewTile = tile:update_tile(Tile),
  gen_server:cast(Scenario, {update_map, {Map, Vertex, NewTile}}).

update_map(Map, Vertex, Tile) ->
  digraph:add_vertex(Map, Vertex, Tile).

update_map(Map, Vertex, Attr, Value, Scenario) ->
  {Vertex, Tile} = digraph:vertex(Map, Vertex),
  case Attr of
    character ->
      case Value of
        nil ->
          NewTile = dict:store(character, nil, Tile);
        Character ->
          NewTile = dict:store(character, Character, Tile)
      end;
    value ->
      NewTile = dict:store(value, Value, Tile);      
    Attr ->
      io:format("No update function for: ~p~n",[Attr]),
      NewTile = Tile
  end,
  NewTile2 = tile:update_tile(NewTile),
  gen_server:cast(Scenario, {update_map, {Map, Vertex, NewTile2}}).

check_objectives({{Players, Zombies, _}, _, _, {{T,ZL,{CLength,CLivingLength}}, _}} = State) ->
  case ZL < 1 of
    true ->
      {stop, normal, {Players, Zombies}};
    false ->
      case CLength >= 1 andalso CLivingLength =< 0 of
        true ->
          {stop, normal, {Players, Zombies}};
        false ->
          TimeNow = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
          case TimeNow - T >= 1800 of
            true ->
              {stop, normal, {Players, Zombies}};
            false ->
              {noreply, State}
          end
      end
  end.

handle_call({create_character, {Name, Class}}, _From, State) ->
  {Characters, Map, {CP, ZP}, {{T, ZL, {CLength, CLivingLength}}, B}} = State,
  {Player, Zombie, TickList} = Characters,
  Character = spawn_character(player, Map, {Player, Zombie}, CP, [Name, Class]),
  NewList = [Character|TickList],
  NewState = {{Player, Zombie, NewList}, Map, {CP,ZP}, {{T, ZL, {CLength + 1, CLivingLength + 1}}, B}},
  {reply, Character, NewState};

handle_call(Request, _From, State) ->
  io:format("Scenario received unknown call: ~p when State was: ~p~n~n~n",[Request,
      State]),
  Reply = ok,
  {reply, Reply, State}.

handle_cast({add_to_tick_list, Pid}, State) ->
  {{CL, ZL, TickList}, M, SP, O} = State,
  NewList = [Pid|TickList],
  {noreply, {{CL, ZL, NewList}, M, SP, O}};

handle_cast(tick, State) ->
  {{_, _, TickList}, _, _, _} = State,
  lists:foreach(fun(Character) -> gen_server:cast(Character, tick) end, TickList),
  check_objectives(State);

handle_cast({wait, Pid}, State) ->
  gen_server:cast(Pid, unlock),
  {noreply, State};

handle_cast({update_map, {G, V, D}}, State) ->
  update_map(G, V, D),
  {noreply, State};
  
handle_cast({update_board, Character}, State) ->
  {{Players, Zombies, TL}, M, SP, {O, Board}} = State,
  Name = character:lookup(Character, tag),
  Kills = character:lookup(Character, kills),
  Living = character:lookup(Character, living),
  Board1 = lists:keydelete(Name,1, Board),
  NewBoard = [{Name, "- ",integer_to_list(Kills)," ",Living,"<br/>"}|Board1],
  CharStats = lists:flatmap(fun(CharStat)-> tuple_to_list(CharStat) end, NewBoard),
  FlatBoard = lists:concat(["Players-<br/>", CharStats, "Zombies- ",
      ets:info(Zombies, size)]),
  lists:foreach(
    fun({Char}) ->
        Pid = character:lookup(Char, id),
        gen_server:cast(Pid, {update_board, FlatBoard})
    end,
    ets:tab2list(Players)),
  {noreply, {{Players, Zombies, TL}, M, SP, {O, NewBoard}}};

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
