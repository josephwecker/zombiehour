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
  gen_server:cast(Scenario, {add_character, self()}),
  {ok, {Char, inactive, Scenario}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Character, Address, Scenario}) ->
  Feedback = dict:fetch(feedback, Character),
  case Address of
    inactive ->
      ok;
    Address ->
      case Feedback of
        empty ->
          ok;
        Msg ->
          Address ! Msg,
          gen_server:cast(self(), {update_character, {feedback, empty}})
      end
  end,
  {noreply, {Character, Address, Scenario}};

handle_cast({update_character, {Attr, Value}}, {Character, Address, Scenario}) ->
  NewCharacter = dict:store(Attr, Value, Character),
  {noreply, {NewCharacter, Address, Scenario}};

handle_cast({hear, Msg}, {Character, Address, Scenario}) ->
  Feedback = dict:fetch(feedback, Character),
  case Feedback of
    empty ->
      NewFeedback = lists:concat([Msg, "<br/>\n"]);
    Feedback ->
      NewFeedback = lists:concat([Feedback, Msg, "<br/>\n"])
  end,
  gen_server:cast(self(), {update_character, {feedback, NewFeedback}}),
  {noreply, {Character, Address, Scenario}};

handle_cast({return_address, Address}, State) ->
  {Attr, _, Scenario} = State,
  {noreply, {Attr, Address, Scenario}};

handle_cast({post, {Param, Value}}, {Character, _, Scenario} = State) ->
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
