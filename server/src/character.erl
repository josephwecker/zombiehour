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
  [XStr, YStr] = string:tokens(dict:fetch(location, Character), "XY"),
  X = list_to_integer(XStr),
  Y = list_to_integer(YStr),
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
                      [integer_to_list(Mem)|","]
                  end;
                true ->
                  {Key, Tile} = digraph:vertex(ScenarioMap, Key),
                  case dict:fetch(blocking, Tile) of
                    true ->
                      "2,";
                    false ->
                      "1,"
                  end
              end
          end,
          lists:seq(X-12,X+12)))
    end,
    lists:seq(Y-12,Y+12))).

learn_tiles(Map, Tiles) ->
  lists:sort(lists:map(
    fun(Key) ->
        {Key, Tile} = digraph:vertex(Map, Key),
        case dict:fetch(blocking, Tile) of
          true ->
            Sym = 4;
          false ->
            Sym = 3
        end,
        {Key, Sym}
    end,
    Tiles)).


init([Name, Scenario]) ->
  C = dict:new(),
  Ch = dict:store(id, self(), C),
  Cha = dict:store(tag, Name, Ch),
  Char = dict:store(feedback, empty, Cha),
  Chara = dict:store(sight, 8, Char),
  Charac = dict:store(visible_tiles, [], Chara),
  Charact = dict:store(known_tiles, [], Charac),
  gen_server:cast(Scenario, {add_character, self()}),
  Feedback = [],
  Addresses = {inactive, Scenario},
  {ok, {Charact, Feedback, Addresses}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {C, Feedback, {Address, S}}) ->
  % Things that need to be flushed to the browser when updates are made:
  %   Map details
  %   Feedback Log
  %   List of "Sounds"
  case Address of
    inactive ->
      NewFeedback = Feedback;
    Address ->
      Map = build_map(C),
      NewFeedback = [],
      case Feedback of
        [] ->
          Data = lists:concat(["{\"map\":\"",Map,"\",\"msg\":\"nil\"}"]);
        Msg ->
          Data = lists:concat(["{\"map\":\"",Map,"\",\"msg\":\"",Msg,"\"}"])
      end,
      Address ! Data
  end,
  {noreply, {C, NewFeedback, {Address, S}}};

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
      KnownTiles = lists:umerge(OldKnownTiles, learn_tiles(Map, VisibleTiles)),
      NewCharacter = dict:store(known_tiles, KnownTiles, C2);
    _ ->
      NewCharacter = dict:store(Attr, Value, Character)
  end,
  {noreply, {NewCharacter, F, A}};

handle_cast({hear, Msg}, {C, Feedback, A}) ->
  %{_, {{_,Old}, Map}} = Feedback,
  %NewMsg = lists:concat([Old, Msg, "<br/>\n"]),
  %NewFeedback = {true, {{true, NewMsg}, Map}},
  NewFeedback = lists:concat([Feedback, Msg, "<br/>"]),
  {noreply, {C, NewFeedback, A}};

handle_cast({return_address, Address}, {C, F, {_, S}}) ->
  {noreply, {C, F, {Address, S}}};

handle_cast({post, {Param, Value}}, {Character, _, {_, Scenario}} = State) ->
  Name = dict:fetch(tag, Character),
  case Param of
    "say" ->
      gen_server:cast(Scenario, {say, lists:concat([Name, ": ", Value])});
    "walk" ->
      gen_server:cast(Scenario, {walk, {Character, Value}});
    Param ->
      io:format("{ ~p, ~p }: failed to match anything.~n",[Param, Value])
  end,
  {noreply, State};

handle_cast(Msg, State) ->
  io:format("cast received: ~p, When state was: ~p~n",[Msg, State]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
