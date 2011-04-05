-module(zombie).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1]).

create(Attrs) ->
  gen_server:start_link(?MODULE, Attrs, []).

observe_characters(Zombie) ->
  ScenarioMap = dict:fetch(map, Zombie),
  VisibleTiles = dict:fetch(visible_tiles, Zombie),
  {X,Y} = nav:position(dict:fetch(location, Zombie)),
  lists:flatten(
  lists:map(
    fun(Row) ->
        lists:flatten(
        lists:map(
          fun(Col) ->
              Key = tile:coords_to_key( Col, Row ),
              case lists:member(Key, VisibleTiles) of
                false ->
                  [];
                true ->
                  {Key, Tile} = digraph:vertex(ScenarioMap, Key),
                  case dict:fetch(characters, Tile) of
                    []->
                      [];
                    Characters ->
                      {{Col, Row}, Characters}
                  end
              end
          end,
          lists:seq(X-7,X+7)))
    end,
    lists:seq(Y-7,Y+7))).

figure_out_what_to_do(Zombie) ->
  case observe_characters(Zombie)of
    [] ->
      RandomDirection = lists:nth(random:uniform(8), ["northwest", "north",
          "northeast", "east", "southeast", "south", "southwest", "west"]),
      %io:format("Zomber meanders aimlessly and goes ~p.  ",[RandomDirection]),
      {walk, {Zombie, RandomDirection}};
    [CharacterList] ->
      MyPosition = nav:position(dict:fetch(location, Zombie)),
      {TargetPosition, List} = CharacterList,
      case nav:distance(MyPosition, TargetPosition) < 2 of
        true ->
          Target = pick_target(List),
          {attack, {Zombie, Target}};
        false ->
          Direction = nav:direction(MyPosition, TargetPosition),
          %io:format("A Zomber Sees you... and goes ~p.  ",[Direction]),
          {walk, {Zombie, Direction}}
      end;
    CharacterLists ->
      MyPosition = nav:position(dict:fetch(location, Zombie)),
      {TargetPosition, List} = find_closest(MyPosition, CharacterLists),
      case nav:distance(MyPosition, TargetPosition) < 2 of
        true ->
          Target = pick_target(List),
          {attack, {Zombie, Target}};
        false ->
          Direction = nav:direction(MyPosition, TargetPosition),
          {walk, {Zombie, Direction}}
      end
  end.

pick_target(List) ->
  [ThisGuy|_] = List,
  ThisGuy.

find_closest(Origin, CharacterLists) ->
  [H|T] = CharacterLists,
  find_closest(Origin, T, H).

find_closest(_Origin, [], BestPick) ->
  BestPick;

find_closest(Origin, Characters, BestPick) ->
  {BPos, _} = BestPick,
  [{HPos, _}=H|T] = Characters,
  BS = nav:distance(Origin, BPos),
  HS = nav:distance(Origin, HPos),
  case HS < BS of
    true ->
      Winner = H;
    false ->
      Winner = BestPick
  end,
  find_closest(Origin, T, Winner).

init([Scenario, Position, Map]) ->
  Attrs = [{id, self()}, {location, Position}, {cooldown, 0}, {map, Map},
    {hp, 20}, {visible_tiles, []}, {sight, 7}, {locked, false},
    {zombified, true}],
  Zombie = dict:from_list(Attrs),
  {ok, {Zombie, Scenario}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Zombie, Scenario}) ->
  Num = dict:fetch(cooldown, Zombie),
  case Num of
    0 ->
      case dict:fetch(locked, Zombie) of
        true ->
          NewZombie = Zombie;
        false ->
          NewZombie = dict:store(locked, true, Zombie),
          ToDo = figure_out_what_to_do(Zombie),
          gen_server:cast(Scenario, ToDo)
      end;
    Num ->
      NewZombie = dict:update_counter(cooldown, -1, Zombie)
  end,
  {noreply, {NewZombie, Scenario}};

handle_cast({update_character, {Attr, Value}}, {Zombie, S}) ->
  case Attr of
    location ->
      Z1 = dict:store(Attr, Value, Zombie),
      VisibleTiles = los:character_los(Z1),
      NewZombie = dict:store(visible_tiles, VisibleTiles, Z1);
      %io:format("Zomber is now at ~p~n",[Value]);
    _ ->
      NewZombie = dict:store(Attr, Value, Zombie)
  end,
  {noreply, {NewZombie, S}};

handle_cast(unlock, {Zombie, S}) ->
  NewZombie = dict:store(locked, false, Zombie),
  {noreply, {NewZombie, S}};

handle_cast({hear, _Msg}, {Z, S}) ->
  % do something like the sound source is intriguing to the zombman, so if
  % he's not doing anything important, than he'll investigate.
  {noreply, {Z, S}};

handle_cast({heat_up, Amount}, {Zombie, S}) ->
  NewZombie = dict:update_counter(cooldown, Amount, Zombie),
  {noreply, {NewZombie, S}};

handle_cast(Msg, State) ->
  io:format("cast received: ~p, When state was: ~p~n",[Msg, State]),
  {noreply, State}.

%temp position:

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
