-module(player).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1]).

create(Attrs) ->
  gen_server:start_link(?MODULE, Attrs, []).

build_map(Character) ->
  ScenarioMap = ets:lookup_element(Character, map, 2),
  VisibleTiles = ets:lookup_element(Character, visible_tiles, 2),
  KnownTiles = ets:lookup_element(Character, known_tiles, 2),
  Location = ets:lookup_element(Character, location, 2),
  {X,Y} = nav:position(Location),
  XO = X - 12,
  YO = Y - 12,
  lists:flatten(
    lists:map(
      fun(Row) ->
          lists:map(
            fun(Col) ->
                Key = tile:coords_to_key( XO + Col, YO + Row ),
                JSKey = Row * 25 + Col + 1,
                case lists:member(Key, VisibleTiles) of
                  false ->
                    case lists:keyfind(Key, 1, KnownTiles) of
                      false ->
                        {JSKey, 0};
                      {Key, Mem} ->
                        {JSKey, Mem}
                    end;
                  true ->
                    {Key, Tile} = digraph:vertex(ScenarioMap, Key),
                    %case dict:fetch(refresh_map, Tile) of
                    %  true ->
                    %    gen_server:cast(self(), {update_character, {location,
                    %          Location}});
                    %  false ->
                    %    ok
                    %end,
                    {JSKey, dict:fetch(symbol, Tile)}
                end
            end,
            lists:seq(0,24))
      end,
      lists:seq(0,24))).

clear_map() ->
  [{Num, 0} || Num <- lists:seq(1,625)].

compare_maps(Map, Map2) ->
  TileList = [Tile || {Tile, Tile2} <- lists:zip(Map, Map2), Tile =/= Tile2],
  create_json_object(TileList).

create_json_object(PropList) ->
  create_json_object(PropList, "{").

create_json_object([{Key, Value}], Result) ->
  Item = lists:concat(["\"", Key, "\":\"", Value, "\"}"]),
  Result ++ Item;

create_json_object([{Key, Value}|PropList], Result) ->
  Item = lists:concat(["\"", Key, "\":\"", Value, "\","]),
  create_json_object(PropList, Result ++ Item).
  
queue_to_string([]) ->
  "ready";

queue_to_string(Queue) ->
  lists:flatmap(
    fun({Action, Value}) ->
        lists:concat([atom_to_list(Action), " ", Value, "<br />"])
    end,
    Queue).

update_queue(Character, Request) ->
  case Request of
    nil ->
      ok;
    clear ->
      ets:insert(Character, [{queuestring, "ready"}, {queue, []}]);
    Request ->
      Queue = ets:lookup_element(Character, queue, 2),
      case Queue of
        [] ->
          NewQueue = [Request];
        _ ->
          NewQueue = lists:append(Queue, [Request])
      end,
      QS = queue_to_string(NewQueue),
      ets:insert(Character, [{queuestring, QS}, {queue, NewQueue}])
  end.

do_request(CharPid, Character, {Action, Value}) ->
  case ets:lookup_element(Character, hp, 2) > 0 of
    true ->
      gen_server:cast(CharPid, {Action, Value});
    false ->
      ok
  end.

update_stat(Atom, {F, M, Stat}) ->
  {F,M, [Atom|Stat]}.

get_msg(Feedback, {Type, Msg}) ->
  case Type of
    combat ->
      lists:concat([Feedback,"<span class='combat_msg'>", Msg, "</span><br/>"]);
    notify ->
      lists:concat([Feedback,"<span class='note_msg'>", Msg, "</span><br/>"]);
    speech ->
      lists:concat([Feedback, Msg,"<br />"]);
    nil ->
      Feedback
  end.

init([Character, Name, Class]) ->
  case Class of
    soldier ->
      ClassAttrs = lists:keysort(1, [{ammo, 60}, {ranged_damage, {2,2}},
          {ranged_acc, 10}]);
    engineer ->
      ClassAttrs = lists:keysort(1, [{repair, 20}]);
    medic ->
      ClassAttrs = lists:keysort(1, [{dress_wounds, 20},{inventory, [pistol,
              first_aid_kit, first_aid_kit, first_aid_kit]}]);
    brawler ->
      ClassAttrs = lists:keysort(1, [{melee_damage, {2,2}}, {melee_acc, 10},
            {maxhp, 24}, {hp, 24}, {speed, 7}])
  end,
  Attrs = lists:keysort(1, [{tag, Name}, {queue, []}, {cooldown, 0}, {maxhp, 20}, {hp, 20},
    {ammo, 20}, {kills,0}, {living, ""}, {board,""}, {queuestring,""},
    {visible_tiles, []}, {known_tiles, []}, {locked, false}, {sight, 8},
    {inventory, [pistol, first_aid_kit]}, {zombified, false}, {speed, 8},
    %Other Attrs
    {melee_acc, 0}, {ranged_acc, 0}, {avoidance, 50}, {melee_damage, {1,2}},
    {ranged_damage, {1,2}},
    %skills:
    {dress_wounds, 10}, {repair, 10}]),
  ets:insert(Character, lists:keymerge(1, Attrs, ClassAttrs)),
  Location = ets:lookup_element(Character, location, 2),
  character:update_character(location, Location, Character),
  Update = {[],clear_map(),[tag,maxhp,hp,ammo,board]},
  Addresses = {inactive, unknown},
  {ok, {Character, Update, Addresses}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Character, Update, {Address, unknown}}) ->
  CharPid = character:lookup(Character, id),
  gen_server:cast(CharPid, update_board),
  {noreply, {Character, Update, {Address, CharPid}}};

handle_cast(tick, {Character, Update, {Address, CharPid}}) ->
  Num = ets:lookup_element(Character, cooldown, 2),
  case Num of
    0 ->
      %case ets:lookup_element(Character, locked, 2) of
      %  true ->
      %    Update1 = Update;
      %  false ->
          case ets:lookup_element(Character, queue, 2) of
            [] ->
              Update1 = Update;
            [Request|NewQueue] ->
              do_request(CharPid, Character, Request),
              QS = queue_to_string(NewQueue),
              Update1 = update_stat(queuestring, Update),
              ets:insert(Character, [{locked, true}, {queuestring, QS}, {queue, NewQueue}])
      %    end
      end;
    Num ->
      Update1 = update_stat(cooldown, Update),
      ets:update_counter(Character, cooldown, -1)
  end,
  case Address of
    inactive ->
      NewAddress = Address,
      NewUpdate = Update1;
    Address ->
      {Feedback, Map, Stats} = Update1,
      Map2 = build_map(Character),
      case Map2 == Map of
        true ->
          MapData = "\"nil\"",
          NewMap  = Map;
        false ->
          MapData = compare_maps(Map2, Map),
          NewMap  = Map2
      end,
      NewFeedback = [],
      case Feedback of
        [] ->
          FeedbackMsg = "nil";
        FeedbackMsg ->
          ok
      end,
      NewStats = [],
      case Stats of
        [] ->
          StatData = "\"nil\"";
        Stats ->
          StatsPropList = lists:map(
            fun(Stat) ->
                {Stat, ets:lookup_element(Character, Stat, 2)}
            end,
            Stats),
          StatData = create_json_object(StatsPropList)
      end,
      case MapData == "nil" andalso StatData == "nil" andalso FeedbackMsg == "nil" of
        true ->
          NewAddress = Address;
        false ->
          JSON =
          lists:concat(["{\"map\":",MapData,",\"data\":",StatData,",\"msg\":\"",FeedbackMsg,"\"}"]),
          NewAddress = inactive,
          Address ! JSON
      end,
      NewUpdate = {NewFeedback, NewMap, NewStats}
  end,
  {noreply, {Character, NewUpdate, {NewAddress, CharPid}}};

handle_cast({return_address, Address}, {P, U, {_, S}}) ->
  {noreply, {P, U, {Address, S}}};

handle_cast({update_board, Board}, {Character, U, A}) ->
  ets:insert(Character, {board, Board}),
  NewUpdate = update_stat(board, U),
  {noreply, {Character, NewUpdate, A}};

handle_cast({update_stat, Stat}, {C, U, A}) ->
  NewUpdate = update_stat(Stat, U),
  {noreply, {C, NewUpdate, A}};

handle_cast(update_all, {Character, {F, _Map, _Attrs}, A}) ->
  Attrs = [tag, ammo, maxhp, hp, board],
  {noreply, {Character, {F, clear_map(), Attrs}, A}};

handle_cast({post, {Param, Value}}, {Character, {Feedback, M, S}, A}) ->
  case Param of
    "say" ->
      NewFeedback = Feedback,
      Request = nil,
      {_, CharPid} = A,
      gen_server:cast(CharPid, {say, Value});
    "walk" ->
      NewFeedback = Feedback,
      Request = {walk, Value};
    "shoot" ->
      case lists:member(pistol, ets:lookup_element(Character, inventory, 2)) of
        true ->
          NewFeedback = Feedback,
          Request = {shoot, Value};
        false ->
          Request = nil,
          NewFeedback = get_msg(Feedback, {notify, "You don't have a gun equipped."})
      end;
    "dress_wound" ->
      case lists:member(first_aid_kit, character:lookup(Character, inventory)) of
        true ->
          NewFeedback = Feedback,
          Request = {dress_wound, Value};
        false ->
          Request = nil,
          NewFeedback = get_msg(Feedback, {notify, "You don't have anything to dress wounds with."})
      end;
    "open" ->
      NewFeedback = Feedback,
      Request = {open, Value};
    "close" ->
      NewFeedback = Feedback,
      Request = {close, Value};
    "repair" ->
      NewFeedback = Feedback,
      Request = {repair,Value};
    "cancel" ->
      NewFeedback = Feedback,
      Request = clear;
    Param ->
      NewFeedback = Feedback,
      Request = nil,
      io:format("{ ~p, ~p }: failed to match anything.~n",[Param, Value])
  end,
  update_queue(Character, Request),
  NewUpdate = update_stat(queuestring, {NewFeedback, M, S}),
  {noreply, {Character, NewUpdate, A}};

handle_cast({msg, Msg}, {C, {Feedback, M, S}, A}) ->
  NewFeedback = get_msg(Feedback, Msg),
  {noreply, {C, {NewFeedback, M, S}, A}};

handle_cast(stop, _State) ->
  {stop, normal, scenario_closed};

handle_cast(Msg, State) ->
  io:format("Player received unknown cast: ~p, when state was: ~p~n",[Msg, State]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  gen_server:cast(zhandler, {close_character, self()}),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
