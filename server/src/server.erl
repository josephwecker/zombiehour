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
  code:purge(player),
  code:load_file(player),
  code:purge(character),
  code:load_file(character),
  code:purge(scenario),
  code:load_file(scenario),
  code:purge(zombie),
  code:load_file(zombie),
  code:purge(tile),
  code:load_file(tile),
  code:purge(nav),
  code:load_file(nav),
  code:purge(los),
  code:load_file(los),
  code:purge(tick),
  code:load_file(tick),
  zhandler:start([8080]).

