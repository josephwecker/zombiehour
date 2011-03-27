-module(character).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1]).

create(Attrs) ->
  gen_server:start_link(?MODULE, Attrs, []).

init([Name, Scenario]) ->
  Feedback = empty,
  gen_server:cast(Scenario, {add_character, self()}),
  {ok, {{Name, Feedback}, inactive, Scenario}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Character, Address, Scenario}) ->
  {Name, Feedback} = Character,
  case Address of
    inactive ->
      NewFeedback = Feedback;
    Address ->
      case Feedback of
        empty ->
          %Address ! "hi";
          NewFeedback = Feedback;
        Msg ->
          Address ! Msg,
          NewFeedback = empty
      end
  end,
  {noreply, {{Name, NewFeedback}, Address, Scenario}};

handle_cast({hear, Msg}, {Attr, Address, Scenario}) ->
  {Name, Feedback} = Attr,
  case Feedback of
    empty ->
      NewFeedback = lists:concat([Msg, "<br/>\n"]);
    Feedback ->
      NewFeedback = lists:concat([Feedback, Msg, "<br/>\n"])
  end,
  {noreply, {{Name, NewFeedback}, Address, Scenario}};

handle_cast({return_address, Address}, State) ->
  {Attr, _, Scenario} = State,
  {noreply, {Attr, Address, Scenario}};

handle_cast({post, {Param, Value}}, {{Name,_}, _, Scenario} = State) ->
  case Param of
    "say" ->
      gen_server:cast(Scenario, {say, lists:concat([Name, ": ", Value])});
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
