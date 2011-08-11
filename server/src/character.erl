-module(character).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1, lookup/2, find_target/1, find_target/2, update_character/3]).

create([Scenario, Map, Characters, SpawnPoints, TypeAttrs]) ->
  Position = lists:nth(random:uniform(length(SpawnPoints)), SpawnPoints),
  Character = ets:new(character_sheet, [set, public]),
  ets:insert(Character, [{map, Map}, {location, Position}, {moved, false}]),
  {Players, Zombies} = Characters,
  {Type, Attrs} = TypeAttrs,
  case Type of
    player ->
      {ok, Pid} = player:create([Character|Attrs]),
      Table = Players;
    zombie ->
      {ok, Pid} = zombie:create([Character|Attrs]),
      gen_server:cast(Scenario, {add_to_tick_list, Pid}),
      Table = Zombies
  end,
  {ok, CPid} = gen_server:start_link(?MODULE, [Character, Pid, Scenario, Characters], []),
  ets:insert(Character, {id, CPid}),
  ets:insert(Table, {Character, Position, quick_info}),
  scenario:update_map(Map, Position, character, Character, self()),
  Pid.

lookup(Tab, Atom) ->
  ets:lookup_element(Tab, Atom, 2).

find_target(Character) ->
  Tiles = lookup(Character, visible_tiles),
  target(Character, Tiles).

find_target(Character, Direction) ->
  Origin = lookup(Character, location),
  Tiles = nav:get_quadrant(Origin, Direction, lookup(Character, sight)),
  VisibleTiles = lookup(Character, visible_tiles),
  TargetTiles = [ X || X <- Tiles, lists:member(X, VisibleTiles) ],
  target(Character, TargetTiles).

target(Character, Tiles) ->
  Map = lookup(Character, map),
  Targets = lists:flatmap(
    fun(Tile) ->
        {Tile, TileData} = digraph:vertex(Map, Tile),
        case dict:fetch(character, TileData) of
          nil ->
            [];
          OtherCharacter ->
            case lookup(OtherCharacter, zombified) /= lookup(Character, zombified) of
              true ->
                [Tile];
              false ->
                []
            end
        end
    end,
    Tiles),
  Origin = lookup(Character, location),
  case Targets of
    [] ->
      false;
    [Target] ->
      Target;
    Targets ->
      find_closest(Origin, Targets)
  end.

find_closest(Origin, Characters) ->
  %io:format("~p~p~n",[Origin, Characters]),
  [H|T] = Characters,
  find_closest(Origin, T, H).

find_closest(_Origin, [], BestPick) ->
  BestPick;

find_closest(Origin, Characters, BestPick) ->
  [H|T] = Characters,
  BS = nav:distance(Origin, BestPick),
  HS = nav:distance(Origin, H),
  case HS < BS of
    true ->
      Winner = H;
    false ->
      Winner = BestPick
  end,
  find_closest(Origin, T, Winner).

learn_tiles(Map, Tiles) ->
  lists:keysort(1, lists:map(
    fun(Key) ->
        {Key, Tile} = digraph:vertex(Map, Key),
        Sym = lists:concat([dict:fetch(symbol, Tile),"_shaded"]),
        {Key, Sym}
    end,
    Tiles)).

update_character(Attr, Value, Character) ->
  ets:insert(Character, {Attr, Value}),
  case Attr of
    location ->
      VisibleTiles = los:character_los(Character),
      case lookup(Character, zombified) of
        true ->
          ets:insert(Character, {visible_tiles, VisibleTiles});
        false ->
          OldKnownTiles = lookup(Character, known_tiles),
          Map = lookup(Character, map),
          KnownTiles = lists:ukeymerge(1, learn_tiles(Map, VisibleTiles), OldKnownTiles),
          ets:insert(Character, [{visible_tiles, VisibleTiles}, {known_tiles, KnownTiles}])
      end;
    _ ->
      ok
  end.

die({Character, {Scenario, _}, {PlayerList, ZombieList}}) ->
  ets:insert(Character, {living, "deceased"}),
  case lookup(Character, zombified) of
    true ->
      ets:delete(ZombieList, self());
    false ->
      ets:delete(PlayerList, self())
  end,
  Location = lookup(Character, location),
  Map = lookup(Character, map),
  scenario:update_map(Map, Location, character, nil, Scenario),
  case lookup(Character, zombified) of
    true ->
      ok;
    false ->
      gen_server:cast(Scenario, {update_board, Character})
  end.

get_kill(Character, Scenario) ->
  case lookup(Character, zombified) of
    true ->
      ok;
    false ->
      ets:update_counter(Character, kills, 1),
      gen_server:cast(Scenario, {update_board, Character})
  end.

unlock(Character) ->
  ets:insert(Character, {locked, false}).

heat_up(Pid, Character, Amount) ->
  gen_server:cast(Pid, {update_stat, cooldown}),
  ets:update_counter(Character, cooldown, Amount).

init([Character, Pid, Scenario, CharLists]) ->
  Addresses = {Scenario, Pid},
  State = {Character, Addresses, CharLists},
  {ok, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({update_character, {Attr, Value}}, State) ->
  {Character, _, _} = State,
  update_character(Attr, Value, Character),
  {noreply, State};

handle_cast({take_damage, Amt}, State) ->
  {Character, {Scenario, Pid}, _} = State,
  ets:update_counter(Character, hp, -Amt),
  gen_server:cast(Pid, {update_stat, hp}),
  NewHP = lookup(Character, hp),
  case NewHP >= 1 of
    true ->
      scenario:update_map(Character, Scenario);
    false ->
      die(State)
  end,
  {noreply, State};

handle_cast({heal_damage, Amt}, State) ->
  {Character, {Scenario, Pid}, _} = State,
  ets:update_counter(Character, hp, Amt),
  gen_server:cast(Pid, {update_stat, hp}),
  scenario:update_map(Character, Scenario),
  {noreply, State};

handle_cast({destroy_item, Item}, State) ->
  {Character, {Scenario, _}, _} = State,
  Inventory = lookup(Character, inventory),
  NewInventory = lists:delete(Item, Inventory),
  ets:insert(Character, {inventory, NewInventory}),
  %gen_server:cast(Pid, {update_stat, inventory}),
  scenario:update_map(Character, Scenario),
  {noreply, State};

handle_cast(update_board, State) ->
  {Character, {Scenario, _}, _} = State,
  gen_server:cast(Scenario, {update_board, Character}),
  {noreply, State};

handle_cast({update_board, Board}, State) ->
  {_, {_, Pid}, _} = State,
  gen_server:cast(Pid, {update_board, Board}),
  {noreply, State};

handle_cast({hear, Msg}, {P, {Feedback, M, S}, A}) ->
  NewFeedback = lists:concat([Feedback, Msg, "<br/>"]),
  {noreply, {P, {NewFeedback, M, S}, A}};

handle_cast({lose_ammo, Amount}, State) ->
  {Character, {_, Pid}, _} = State,
  ets:update_counter(Character, ammo, -Amount),
  gen_server:cast(Pid, {update_stat, ammo}),
  {noreply, State};

handle_cast({dress_wound, Direction}, State) ->
  {Character, {_, Pid}, _} = State,
  Map = lookup(Character, map),
  Neighbor = nav:neighbor(lookup(Character, location), Direction),
  {Neighbor, NbrTile} = digraph:vertex(Map, Neighbor),
  case dict:fetch(character, NbrTile) of
    nil ->
      gen_server:cast(Pid, {msg, {notify, "There's no one there."}});
    Target ->
      TPid = lookup(Target, id),
      HP = lookup(Target, hp),
      MaxHP = lookup(Target, maxhp),
      case HP >= MaxHP of
        true ->
          gen_server:cast(Pid, {msg, {notify, "They don't have any wounds to dress."}});
        false ->
          HealAmount = 6,
          case HP + HealAmount >= MaxHP of
            true ->
              Amount = MaxHP - HP,
              gen_server:cast(TPid, {heal_damage, Amount});
            false ->
              gen_server:cast(TPid, {heal_damage, HealAmount})
          end,
          gen_server:cast(self(), {destroy_item, first_aid_kit})
      end
  end,
  heat_up(Pid, Character, 26),
  unlock(Character),
  {noreply, State};

handle_cast({attack, Direction}, State) ->
  {Character, {Scenario, Pid}, _} = State,
  Map = lookup(Character, map),
  Neighbor = nav:neighbor(lookup(Character, location), Direction),
  {Neighbor, NbrTile} = digraph:vertex(Map, Neighbor),
  case dict:fetch(character, NbrTile) of
    nil ->
      case dict:fetch(structure, NbrTile) of
        nil ->
          gen_server:cast(Pid, {msg, {combat, "You swing at the open air."}});
        _Structure ->
          case random:uniform(100)+lookup(Character, ranged_acc) >= 50 of
            false ->
              gen_server:cast(Pid, {msg, {combat, "You fail to damage it."}});
            true ->
              {Base, Range} = lookup(Character, melee_damage),
              Damage = Base + random:uniform(Range),
              gen_server:cast(Pid, {msg, {combat, "You hit it."}}),
              NewTile = tile:damage_tile(NbrTile, Damage),
              gen_server:cast(Scenario, {update_map, {Map, Neighbor, NewTile}})
          end
      end;
    Target ->
      TPid = lookup(Target, id),
      case random:uniform(100)+lookup(Character, ranged_acc) >=
        lookup(Target, avoidance) of
        false ->
          gen_server:cast(Pid, {msg, {combat, "You miss."}}),
          gen_server:cast(TPid, {msg, {combat, "The Zombie swings at you but misses."}});
        true ->
          {Base, Range} = lookup(Character, melee_damage),
          Damage = Base + random:uniform(Range),
          gen_server:cast(TPid, {take_damage, Damage}),
          case lookup(Target, hp) =< Damage of
            true ->
              gen_server:cast(Pid, {msg, {combat, "You hit and kill your opponent."}}),
              get_kill(Character, Scenario),
              gen_server:cast(TPid, {msg, {combat, lists:concat(["You take <span class='dmg'>", 
                      Damage, " damage</span> and <span class='dmg'>die</span>."])}});
            false ->
              gen_server:cast(Pid, {msg, {combat, "You hit."}}),
              gen_server:cast(TPid, {msg, {combat,  lists:concat(["You take <span class='dmg'>",
                        Damage, " damage</span>."])}})
          end
      end
  end,
  heat_up(Pid, Character, 16),
  unlock(Character),
  {noreply, State};

handle_cast({shoot, Direction}, State) ->
  {Character, {Scenario, Pid}, _} = State,
  Tile = character:find_target(Character, Direction),
  case lookup(Character, ammo) >= 1 of
    true ->
      case Tile of
        false ->
          gen_server:cast(Pid, {msg, {notify, "There are no targets in that direction."}});
        Tile ->
          Map = lookup(Character, map),
          {Tile, TargetTile} = digraph:vertex(Map, Tile),
          Target = dict:fetch(character, TargetTile),
          case Target of
            nil ->
              gen_server:cast(Pid, {msg, {combat, "Your target evades."}})
              ;
            Target ->
              TPid = lookup(Target, id),
              case random:uniform(100)+lookup(Character, ranged_acc)
                >= lookup(Target, avoidance) of
                false ->
                  gen_server:cast(Pid, {msg, {combat, "Your shot misses."}});
                true ->
                  {Base, Range} = lookup(Character, ranged_damage),
                  Damage = Base + random:uniform(Range),
                  gen_server:cast(TPid, {take_damage, Damage}),
                  case lookup(Target, hp) =< Damage of
                    true ->
                      gen_server:cast(Pid, {msg, {combat, "You shoot and kill your opponent."}}),
                      get_kill(Character, Scenario);
                    false ->
                      gen_server:cast(Pid, {msg, {combat, "You shot the Zomber."}})
                  end
              end
          end,
          gen_server:cast(self(), {lose_ammo, 1}),
          heat_up(Pid, Character, 16)
      end;
    false ->
      gen_server:cast(Pid, {msg, {notify, "You don't have any ammunition left."}})
  end,
  unlock(Character),
  {noreply, State};

handle_cast({walk, Direction}, State) ->
  {Character, {Scenario, Pid}, _} = State,
  case Direction of
    "" ->
      unlock(Character);
    Direction ->
      DesiredLocation = nav:neighbor(lookup(Character, location), Direction),
      Map = lookup(Character, map),
      case digraph:vertex(Map, DesiredLocation) of
        false ->
          unlock(Character);
        {_, TileData} ->
          case dict:fetch(movement, TileData) of
            false ->
              case dict:fetch(character, TileData) of
                nil ->
                  case dict:fetch(structure, TileData) of
                    nil ->
                      unlock(Character);
                    _ ->
                      gen_server:cast(self(), {attack, Direction})
                  end;
                Target ->
                  case lookup(Target, zombified) == lookup(Character, zombified) of
                      true ->
                        unlock(Character);
                      false ->
                        gen_server:cast(self(), {attack, Direction})
                    end
              end;
            Cost ->
              Location = lookup(Character, location),
              NewLocation = DesiredLocation,
              scenario:update_map(Map, Location, character, nil, Scenario),
              scenario:update_map(Map, NewLocation, character, Character, Scenario),
              Speed = lookup(Character, speed),
              update_character(location, NewLocation, Character),
              case length(Direction) > 5 of
                true ->
                  Cooldown = round((Speed + Cost) * 1.41);
                false ->
                  Cooldown = Speed + Cost
              end,
              update_character(moved, {Direction, Cooldown}, Character),
              %from here
              %Alert = lists:concat(["walk_", Direction]),
              %gen_server:cast(Pid, {add_alert, {Alert, Speed + Cost}}),
              %to here
              heat_up(Pid, Character, Cooldown),
              unlock(Character)
          end
      end
  end,
  {noreply, State};

handle_cast({open, Direction}, State) ->
  {Character, {Scenario, Pid}, _} = State,
  Location = lookup(Character, location),
  Target = nav:neighbor(Location, Direction),
  Map = lookup(Character, map),
  {Target, TileData} = digraph:vertex(Map, Target),
  case dict:fetch(structure,TileData) of
    nil ->
      gen_server:cast(Pid, {msg, {notify, "There's nothing to open there."}});
    _Structure ->
      NewTile = tile:open(TileData),
      gen_server:cast(Scenario, {update_map, {Map, Target, NewTile}}),
      heat_up(Pid, Character, 8)
  end,
  unlock(Character),
  {noreply, State};

handle_cast({close, Direction}, State) ->
  {Character, {Scenario, Pid}, _} = State,
  Location = lookup(Character, location),
  Target = nav:neighbor(Location, Direction),
  Map = lookup(Character, map),
  {Target, TileData} = digraph:vertex(Map, Target),
  case dict:fetch(structure,TileData) of
    nil ->
      gen_server:cast(Pid, {msg, {notify, "There's nothing to close there."}});
    _Structure ->
      NewTile = tile:close(TileData),
      gen_server:cast(Scenario, {update_map, {Map, Target, NewTile}}),
      heat_up(Pid, Character, 8)
  end,
  unlock(Character),
  {noreply, State};

handle_cast({repair, Direction}, State) ->
  {Character, {Scenario, Pid}, _} = State,
  Location = lookup(Character, location),
  Target = nav:neighbor(Location, Direction),
  Map = lookup(Character, map),
  {Target, TileData} = digraph:vertex(Map, Target),
  case dict:fetch(structure,TileData) of
    nil ->
      gen_server:cast(Pid, {msg, {notify, "There's nothing to repair there."}});
    _Structure ->
      NewTile = tile:repair_tile(TileData,2),
      gen_server:cast(Scenario, {update_map, {Map, Target, NewTile}}),
      heat_up(Pid, Character, 16)
  end,
  unlock(Character),
  {noreply, State};

handle_cast({say, Msg}, State) ->
  {Character, {Scenario, _}, _} = State,
  Name = lookup(Character, tag),
  Output = {speech, lists:concat([Name, ": ", Msg])},
  gen_server:cast(Scenario, {make_noise, {Character, 10, talking, Output}}),
  {noreply, State};

handle_cast({observe_scene, {Location, Scene, Msg}}, State) ->
  {Character, _, _} = State,
  Pid = lookup(Character, id),
  gen_server:cast(Pid, {msg, Msg}),
  {noreply, State};

handle_cast({observe_noise, {Location, Volume, Noise, Msg}}, State) ->
  {Character, _, _} = State,
  Pid = lookup(Character, id),
  gen_server:cast(Pid, {msg, Msg}),
  {noreply, State};

handle_cast({msg, Msg}, State) ->
  {_, {_, Player}, _} = State,
  gen_server:cast(Player, {msg, Msg}),
  {noreply, State};

handle_cast(Msg, State) ->
  io:format("Character received unknown cast: ~p, when state was: ~p~n",[Msg, State]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
