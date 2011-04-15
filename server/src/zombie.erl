-module(zombie).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1]).

figure_out_what_to_do(Zombie) ->
  case character:find_target(Zombie) of
    false ->
      Direction = lists:nth(random:uniform(8), ["northwest", "north",
          "northeast", "east", "southeast", "south", "southwest", "west"]);
    Target ->
      MyPosition = dict:fetch(location, Zombie),
      Direction = nav:direction(MyPosition, Target)
  end,
  {walk, {Zombie, Direction}}.

create(Attrs) ->
  gen_server:start_link(?MODULE, Attrs, []).

init([Scenario, Position, Map]) ->
  Attrs = [{id, self()}, {location, Position}, {cooldown, 0}, {map, Map},
    {tag, "Zomber"}, {hp, 12}, {visible_tiles, []}, {sight, 7}, {locked, false},
    {zombified, true}],
  Zombie = dict:from_list(Attrs),
  {ok, {Zombie, Scenario}}.

handle_call(character, _From, {Zombie, S}) ->
  {reply, Zombie, {Zombie, S}};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Zombie, Scenario}) ->
  case dict:fetch(hp, Zombie) > 0 of
    true ->
      Num = dict:fetch(cooldown, Zombie),
      case Num of
        0 ->
          case dict:fetch(locked, Zombie) of
            true ->
              NewZombie = Zombie;
            false ->
              NewZombie = dict:store(locked, true, Zombie),
              ToDo = figure_out_what_to_do(Zombie),
  %io:format("~p~n",[ToDo]),
              gen_server:cast(Scenario, ToDo)
          end;
        Num ->
          NewZombie = dict:update_counter(cooldown, -1, Zombie)
      end;
    false ->
      NewZombie = Zombie
  end,
  {noreply, {NewZombie, Scenario}};

handle_cast({take_damage, Amt}, {Zombie, Scenario}) ->
  NewZombie = dict:update_counter(hp, -Amt, Zombie),
  case dict:fetch(hp, NewZombie) >= 1 of
    true ->
      gen_server:cast(Scenario, {update_self, NewZombie});
    false ->
      gen_server:cast(Scenario, {die, NewZombie})
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

handle_cast({msg, _Msg}, {Z, S}) ->
  {noreply, {Z, S}};

handle_cast(get_kill, State) ->
  {noreply, State};

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
