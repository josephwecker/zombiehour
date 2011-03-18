-module(zhandler).
-export([start/1, stop/0]).

start([Port]) ->
	misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]).

stop() ->
	misultin:stop().

handle_http(Req) ->
  io:format("~p~n", [Req]),
	handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).


handle('GET', ["command_me"], Req) ->
	Req:stream(head, [{"Content-Type", "text/plain"}]),
	Req:stream("1"),
	timer:sleep(2000),
	Req:stream("2"),
	timer:sleep(2000),
	Req:stream("3"),
	Req:stream(close);

handle('GET', ["command_thee"], Req) ->
  io:format("Ah, so you want me to ~p~n", [Req]),
  Req:ok([{"Content-Type","text/plain"}], "Fine.");

handle(_,_,Req) ->
	Req:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").
