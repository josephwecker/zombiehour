-module(chandler).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1]).

create(Attrs) ->
  gen_server:start_link(?MODULE, Attrs, []).

init([Name, Scenario]) ->
  {ok, Character} = character:create([self(), Scenario]),
  Feedback = empty,
  {ok, {{Name, Feedback}, Character, inactive}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {{Name, Feedback}, Character, Address}) ->
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
  {noreply, {{Name, NewFeedback}, Character, Address}};

handle_cast({hear, Msg}, {Attr, Character, Address}) ->
  {Name, Feedback} = Attr,
  case Feedback of
    empty ->
      NewFeedback = lists:concat([Msg, "<br/>\n"]);
    Feedback ->
      NewFeedback = lists:concat([Feedback, Msg, "<br/>\n"])
  end,
  {noreply, {{Name, NewFeedback}, Character, Address}};

handle_cast({return_address, Address}, State) ->
  {Attr, Character, _} = State,
  {noreply, {Attr, Character, Address}};

handle_cast(Msg, {{Name,_}, Character, _} = State) ->
  case re:run(Msg,"say .*") of
    {match, _} ->
      NewMsg = lists:concat([Name, ": ", string:substr(Msg, 5)]),
      gen_server:cast(Character, {say, NewMsg});
    nomatch ->
     io:format("~p~n",[Msg])
  end,
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
