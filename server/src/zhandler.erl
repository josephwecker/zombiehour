-module(zhandler).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, stop/0]).
-export([do_loop/2]).

start(Attrs) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Attrs, []).

stop() ->
	gen_server:cast(?MODULE, stop).

init([Port]) ->
  % trap_exit -> this gen_server needs to be supervised
  process_flag(trap_exit, true),
  % start misultin & set monitor
  misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]),
  erlang:monitor(process, misultin),
  {ok, Port}.

handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

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
  io:format("~p~n", [Req]),
	handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).


handle('GET', ["command_me"], Req) ->
	Req:stream(head, [{"Content-Type", "text/plain"}]),
	Req:stream("1"),
	Req:stream("1.5"),
	timer:sleep(5000),
	Req:stream("2"),
	timer:sleep(5000),
	Req:stream("3"),
	Req:stream(close);

handle('GET', ["command_thee"], Req) ->
  io:format("Ah, so you want me to ~p~n", [Req]),
  Req:ok([{"Content-Type","text/plain"}], "Fine.");

handle('GET', ["test"], Req) ->
	Req:stream(head, [{"Content-Type", "text/html"}]),
  Req:stream(send_msg("1<br />")),
	timer:sleep(1000),
  Req:stream(send_msg("1.5<br />")),
	timer:sleep(1000),
  Req:stream(send_msg("2<br />")),
	timer:sleep(1000),
  Req:stream(send_msg("<script type='text/javascript'>alert('hi');</script>")),
  Req:stream(close);

handle('GET', ["test2"], Req) ->
	Req:stream(head, [{"Content-Type", "text/html"}]),
  Req:stream(send_msg("<div id='other'>hi</div><script src='http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js' type='text/javascript'></script>")),
  do_loop( Req, 0 );

handle(_,_,Req) ->
	Req:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").

send_msg( Msg ) ->
  string:left(Msg, 1024, $\n).

do_loop( Req, Count) ->
  io:format("~p~n", [Count]),
  Script = lists:concat(["<script type='text/javascript'>$('#other').html('", Count, "');</script>"]),
  Req:stream(send_msg(Script)),
  NewCount = Count + 1,
  case NewCount > 100 of
    true -> 
      Req:stream(close);
    false -> 
      timer:sleep(200),
      do_loop(Req, NewCount)
  end.
