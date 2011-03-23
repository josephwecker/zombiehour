-module(zmaster).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, broadcast/1]).

start(Table) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Table], []).

init([Table]) ->
  Self = self(),
  Ticker = spawn(fun() -> tick:tick(Self, 500) end),
  {ok, {Table, Ticker}}.

handle_call(table, _From, {Table, _} = State) ->
  Reply = Table,
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(tick, State) ->
  {noreply, State};

handle_cast({broadcast, Msg}, {Table, _} = State) ->
  broadcast(Msg, Table),
  {noreply, State};

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

broadcast(Msg) -> 
  gen_server:cast(?MODULE, {broadcast, Msg}).

broadcast(Msg, Table) ->
  List = ets:match(Table, { '_', '$1'}),
  Pids = [ Pid || [Pid] <- List ],
  lists:map( fun(Pid) -> Pid ! Msg end, Pids).
