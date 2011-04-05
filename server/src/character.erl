-module(character).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1]).

create(Attrs) ->
  gen_server:start_link(?MODULE, Attrs, []).

build_map(Character) ->
  ScenarioMap = dict:fetch(map, Character),
  VisibleTiles = dict:fetch(visible_tiles, Character),
  KnownTiles = dict:fetch(known_tiles, Character),
  {X,Y} = nav:position(dict:fetch(location, Character)),
  %traverse map...
  %traverse rows...
 % CharMap =
  lists:flatten(
  lists:map(
    fun(Row) ->
        %traverse cols...
        lists:flatten(
        lists:map(
          fun(Col) ->
              Key = tile:coords_to_key( Col, Row ),
              case lists:member(Key, VisibleTiles) of
                false ->
                  case lists:keyfind(Key, 1, KnownTiles) of
                    false ->
                      "0,";
                    {Key, Mem} ->
                      lists:concat([Mem,","])
                  end;
                true ->
                  {Key, Tile} = digraph:vertex(ScenarioMap, Key),
                  lists:concat([dict:fetch(symbol, Tile), ","])
              end
          end,
          lists:seq(X-12,X+12)))
    end,
    lists:seq(Y-12,Y+12))).

learn_tiles(Map, Tiles) ->
  lists:keysort(1, lists:map(
    fun(Key) ->
        {Key, Tile} = digraph:vertex(Map, Key),
        Sym = integer_to_list(list_to_integer(dict:fetch(symbol, Tile))+8),
        {Key, Sym}
    end,
    Tiles)).

update_queue(Character, Request) ->
  case Request of
    nil ->
      Character;
    Request ->
      Queue = dict:fetch(queue, Character),
      case Queue of
        [] ->
          NewQueue = [Request];
        _ ->
          NewQueue = lists:append(Queue, [Request])
      end,
      dict:store(queue, NewQueue, Character)
  end.

do_request(Scenario, Character, {Action, {character, Value}}) ->
  case dict:fetch(hp, Character) > 0 of
    true ->
      gen_server:cast(Scenario, {Action, {Character, Value}});
    false ->
      ok
  end.

init([Name, Scenario]) ->
  Attrs = [{id, self()}, {tag, Name}, {queue, []}, {cooldown, 0}, {hp, 20},
    {visible_tiles, []}, {known_tiles, []}, {locked, false}, {sight, 8}, {zombified, false}],
  Character = dict:from_list(Attrs),
  gen_server:cast(Scenario, {add_character, self()}),
  Feedback = [],
  Addresses = {inactive, Scenario},
  {ok, {Character, Feedback, Addresses}}.

handle_call(character, _From, {Character, F, A}) ->
  {reply, Character, {Character, F, A}};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Character, Feedback, {Address, Scenario}}) ->
  Num = dict:fetch(cooldown, Character),
  case Num of
    0 ->
      case dict:fetch(locked, Character) of
        true ->
          NewCharacter = Character;
        false ->
          case dict:fetch(queue, Character) of
            [] ->
              NewCharacter = Character;
            [Request|NewQueue] ->
              do_request(Scenario, Character, Request),
              Character1 = dict:store(locked, true, Character),
              NewCharacter = dict:store(queue, NewQueue, Character1)
          end
      end;
    Num ->
      NewCharacter = dict:update_counter(cooldown, -1, Character)
  end,
  case Address of
    inactive ->
      NewFeedback = Feedback;
    Address ->
      Map = build_map(Character),
      NewFeedback = [],
      case Feedback of
        [] ->
          Data =
          lists:concat(["{\"map\":\"",Map,"\",\"flash\":\"",Num,"\",\"msg\":\"nil\"}"]);
        Msg ->
          Data = lists:concat(["{\"map\":\"",Map,"\",\"flash\":\"",Num,"\",\"msg\":\"",Msg,"\"}"])
      end,
      Address ! Data
  end,
  {noreply, {NewCharacter, NewFeedback, {Address, Scenario}}};

handle_cast({add_to_char, {Attr, Value}}, {Character, F, A}) ->
  NewCharacter = dict:store(Attr, Value, Character),
  case Attr of location ->
      gen_server:cast(self(), {update_character, {location, Value}});
    _ ->
      ok
  end,
  {noreply, {NewCharacter, F, A}};

handle_cast({update_character, {Attr, Value}}, {Character, F, A}) ->
  case Attr of
    location ->
      C1 = dict:store(Attr, Value, Character),
      VisibleTiles = los:character_los(C1),
      C2 = dict:store(visible_tiles, VisibleTiles, C1),
      OldKnownTiles = dict:fetch(known_tiles, Character),
      Map = dict:fetch(map, Character),
      KnownTiles = lists:ukeymerge(1, learn_tiles(Map, VisibleTiles), OldKnownTiles),
      NewCharacter = dict:store(known_tiles, KnownTiles, C2);
    _ ->
      NewCharacter = dict:store(Attr, Value, Character)
  end,
  {noreply, {NewCharacter, F, A}};

handle_cast({take_damage, Amt}, {Character, Feedback, A}) ->
  NewCharacter = dict:update_counter(hp, -Amt, Character),
  HP = dict:fetch(hp, NewCharacter),
  NewFeedback = lists:concat([Feedback, "You take ", Amt, " damage, and now have ",
    HP, " hitpoints<br/>"]),
  {noreply, {NewCharacter, NewFeedback, A}};

handle_cast({hear, Msg}, {C, Feedback, A}) ->
  %{_, {{_,Old}, Map}} = Feedback,
  %NewMsg = lists:concat([Old, Msg, "<br/>\n"]),
  %NewFeedback = {true, {{true, NewMsg}, Map}},
  NewFeedback = lists:concat([Feedback, Msg, "<br/>"]),
  {noreply, {C, NewFeedback, A}};

handle_cast({return_address, Address}, {C, F, {_, S}}) ->
  {noreply, {C, F, {Address, S}}};

handle_cast({heat_up, Amount}, {Character, F, A}) ->
  NewCharacter = dict:update_counter(cooldown, Amount, Character),
  {noreply, {NewCharacter, F, A}};

handle_cast({post, {Param, Value}}, {Character, F, {_, Scenario} = A}) ->
  case Param of
    "say" ->
      Request = nil,
      gen_server:cast(Scenario, {say, {Character, Value}});
    "walk" ->
      Request = {walk, {character, Value}};
    Param ->
      Request = nil,
      io:format("{ ~p, ~p }: failed to match anything.~n",[Param, Value])
  end,
  NewCharacter = update_queue(Character, Request),
  {noreply, {NewCharacter, F, A}};

handle_cast(unlock, {Character, F, A}) ->
  NewCharacter = dict:store(locked, false, Character),
  {noreply, {NewCharacter, F, A}};

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
