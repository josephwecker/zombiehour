-module(server).
-export([start/0, stop/0, restart/0]).

start() ->
  zhandler:start([8080]).

stop() ->
  zhandler:stop().

restart() ->
  zhandler:stop(),
  io:format("~p~n",[os:cmd("./build")]),
  code:purge(zhandler),
  code:load_file(zhandler),
  code:purge(chandler),
  code:load_file(chandler),
  code:purge(character),
  code:load_file(character),
  code:purge(scenario),
  code:load_file(scenario),
  code:purge(tick),
  code:load_file(tick),
  zhandler:start([8080]).

