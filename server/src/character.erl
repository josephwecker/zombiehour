-module(character).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1]).

create(Attrs) ->
  gen_server:start_link(?MODULE, Attrs, []).


init([Name, Scenario]) ->
  C = dict:new(),
  Ch = dict:store(id, self(), C),
  Cha = dict:store(tag, Name, Ch),
  Char = dict:store(feedback, empty, Cha),
  Chara = dict:store(known_tiles, [], Char),
  gen_server:cast(Scenario, {add_character, self()}),
  Feedback = {false, { {false, []}, {false, []} }},
  Addresses = {inactive, Scenario},
  {ok, {Chara, Feedback, Addresses}}.

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
      case Feedback of
        {false, _List} ->
          NewFeedback = Feedback;
        {true, {{true, Msg}, _}} ->
          Address ! Msg,
          gen_server:cast(self(), {update_character, {feedback, empty}}),
          NewFeedback = {false, {{false, []}, {false, []}}}
      end
  end,
  {noreply, {C, NewFeedback, {Address, S}}};

handle_cast({add_location, Location}, {Character, F, A}) ->
  NewCharacter = dict:store(location, Location, Character),
  {noreply, {NewCharacter, F, A}};

handle_cast({update_character, {Attr, Value}}, {Character, F, A}) ->
  case Attr of
    location ->
      OldLocation = dict:fetch(location, Character),
      OldKnownTiles = dict:fetch(known_tiles, Character),
      KnownTiles = lists:append(OldKnownTiles, [OldLocation]),
      Character2 = dict:store(known_tiles, KnownTiles, Character),
      io:format("Known Tiles: ~p~n",[KnownTiles]),
      NewCharacter = dict:store(Attr, Value, Character2);
    _ ->
      NewCharacter = dict:store(Attr, Value, Character)
  end,
  {noreply, {NewCharacter, F, A}};

handle_cast({hear, Msg}, {C, Feedback, A}) ->
  {_, {{_,Old}, Map}} = Feedback,
  NewMsg = lists:concat([Old, Msg, "<br/>\n"]),
  NewFeedback = {true, {{true, NewMsg}, Map}},
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
