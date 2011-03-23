-module(zhandler).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, stop/0]).

start(Attrs) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Attrs, []).

stop() ->
	gen_server:cast(zmaster, stop),
	gen_server:cast(zhandler, stop).

init([Port]) ->
  % trap_exit -> this gen_server needs to be supervised
  process_flag(trap_exit, true),
  % start misultin & set monitor
  misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]),
  erlang:monitor(process, misultin),
  ConnectionTable = ets:new(connections, [set]),
  {ok, Master} = zmaster:start(ConnectionTable),
  {ok, [ConnectionTable, Port, Master]}.

handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast({add_connection, Cookie, Pid}, [Table, _, _] = State) ->
  ets:insert(Table, {Cookie, Pid}),
  %Pid ! "hello",
  {noreply, State};

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
  {noreply, State}.

% handle info when misultin server goes down -> take down misultin_gen_server too [the supervisor will take everything up again]
handle_info({'DOWN', _, _, {misultin, _}, _}, State) ->
  {stop, normal, State};

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    misultin:stop(),
    terminated.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% End GenServer Functions

handle_http(Req) ->
  %io:format("~p~n", [Req]),
  io:format("~p~n", [Req:get(headers)]),
	handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).

handle('GET', ["test"], Req) ->
	Req:stream(head, [{"Content-Type", "text/html"}]),
  Req:stream(ready_msg("1<br />")),
	timer:sleep(1000),
  Req:stream(ready_msg("1.5<br />")),
	timer:sleep(1000),
  Req:stream(ready_msg("2<br />")),
	timer:sleep(1000),
  Req:stream(ready_msg("<script type='text/javascript'>alert('hi');</script>")),
  Req:stream(close);

handle('POST', ["sag"], Req) ->
	Req:respond(204, [], ""),
  Params = Req:parse_post(),
  Gesagt = proplists:get_value("input", Params),
  zmaster:broadcast(Gesagt);

handle('GET', ["test204"], Req) ->
	Req:respond(204, [], "");

handle('GET', ["test2"], Req) ->
	Req:stream(head, [{"Content-Type", "text/html"}]),
  Req:stream(ready_msg("<div id='other'>hi</div><script src='http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js' type='text/javascript'></script>"));

handle('GET', ["test3"], Req) ->
	Req:stream(head, [{"Content-Type", "text/html"}]),
  {'Cookie', Cookie} = lists:keyfind('Cookie', 1, Req:get(headers)),
  gen_server:cast(?MODULE, {add_connection, Cookie, self()}),
  service_loop(Req);

handle('GET', ["test4"], Req) ->
  {'Cookie', Cookie} = lists:keyfind('Cookie', 1, Req:get(headers)),
  gen_server:cast(?MODULE, {add_connection, Cookie, self()}),
  receive
    Msg ->
      Req:respond(200, [{"Content-Type", "text/html"}], ready_msg2( Msg ))
  end;

handle(_,_,Req) ->
	Req:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").

service_loop(Req) ->
  receive
    close ->
      Req:stream(close);
    Msg ->
      Req:stream(ready_msg(Msg)),
      service_loop(Req)
  end.

ready_msg2( Msg ) ->
  ["<div id='data'>", Msg, "</div>"].

ready_msg( Msg ) ->
  NewMsg = lists:concat([ Msg, '<b /><script>alert("hi");</script>j<img src=""></img>']),
  string:left(NewMsg, 4096, $\n).
  %string:left(NewMsg, 2048, $\n).
