-module(zombie).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1]).

figure_out_what_to_do(Character) ->
  CharList = character:lookup(Character, visible_characters),
  case character:find_target(Character, CharList) of
    false ->
      Direction = lists:nth(random:uniform(8), ["northwest", "north",
          "northeast", "east", "southeast", "south", "southwest", "west"]);
    Target ->
      MyPosition = ets:lookup_element(Character, location, 2),
      Direction = nav:direction(MyPosition, Target)
  end,
  {walk, Direction}.

create(Attrs) ->
  gen_server:start_link(?MODULE, Attrs, []).

init([Character]) ->
  process_flag(trap_exit, true),
  Attrs = [{cooldown, 0},
    {tag, "Zomber"}, {hp, 12}, {visible_tiles, []}, {sight, 7}, {locked, false},
    {melee_acc, 0}, {ranged_acc, 0}, {avoidance, 50}, {melee_damage, {1,2}},
    {visible_characters, []}, {zombified, true}, {speed, 18}],
  ets:insert(Character, Attrs),
  %Location = ets:lookup_element(Character, location, 2),
  {ok, {Character, unknown}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Character, unknown}) ->
  CharPid = character:lookup(Character, id),
  {noreply, {Character, CharPid}};

handle_cast(tick, {Character, CharPid}) ->
  case ets:lookup_element(Character, hp, 2) > 0 of
    true ->
      Num = ets:lookup_element(Character, cooldown, 2),
      case Num of
        0 ->
          case ets:lookup_element(Character, locked, 2) of
            true ->
              ok;
            false ->
              ets:insert(Character, {locked, true}),
              ToDo = figure_out_what_to_do(Character),
  %io:format("~p~n",[ToDo]),
              gen_server:cast(CharPid, ToDo)
          end;
        Num ->
          ets:update_counter(Character, cooldown, -1)
      end;
    false ->
      ok
  end,
  {noreply, {Character, CharPid}};

handle_cast({add_alert, _Alert}, State) -> {noreply, State};

handle_cast({msg, _Msg}, State) -> {noreply, State};

handle_cast({update_stat, _Stat}, State) -> {noreply, State};
handle_cast(stop, _State) ->
  {stop, normal, scenario_closed};

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
