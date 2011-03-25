-module(character).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1]).

create(Attrs) ->
  gen_server:start_link(?MODULE, Attrs, []).

init([Chandler, Scenario]) ->
  gen_server:cast(Scenario, {add_character, self()}),
{ok, {Chandler, Scenario}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Chandler, _} = State) ->
  gen_server:cast(Chandler, tick),
  {noreply, State};

handle_cast({say, Msg}, {Chandler, Scenario}) ->
  gen_server:cast(Scenario, {say, Msg}),
  {noreply, {Chandler, Scenario}};

handle_cast({hear, Msg}, {Chandler, Scenario}) ->
  gen_server:cast(Chandler, {hear, Msg}),
  {noreply, {Chandler, Scenario}};

handle_cast(Msg, State) ->
  io:format("~p~n",[Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
