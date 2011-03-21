-module(tick).
-export([tick/2]).

tick(Target, Time) ->
  gen_server:cast(Target, tick),
  timer:sleep(Time),
  tick(Target, Time).
