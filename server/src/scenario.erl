-module(scenario).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, add_character/1]).

start() ->
  gen_server:start_link(?MODULE, [], []).

add_character(Attrs) ->
  character:create(Attrs).

init([]) ->
  Self = self(),
  Characters = [],
  Ticker = spawn(fun() -> tick:tick(Self, 200) end),
  {ok, {Characters, Ticker}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, {Characters, _} = State) ->
  lists:foreach(fun(Character) -> gen_server:cast(Character, tick) end, Characters),
  {noreply, State};

handle_cast({say, Msg}, {Characters, _} = State) ->
  lists:foreach(fun(Character) -> gen_server:cast(Character, {hear, Msg}) end, Characters),
  {noreply, State};

handle_cast({add_character, Character}, {Characters, Ticker}) ->
  NewList = lists:append(Characters,[Character]),
  {noreply, {NewList, Ticker}};

handle_cast(Msg, State) ->
  io:format("~p~n",[Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, {_, Ticker}) ->
  exit(Ticker, game_over),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
