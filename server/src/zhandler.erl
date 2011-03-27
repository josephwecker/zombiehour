-module(zhandler).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([broadcast/1, start/1, stop/0]).

start(Attrs) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Attrs, []),
  create_scenario().

stop() ->
	gen_server:cast(?MODULE, stop).

broadcast(Msg) -> 
  gen_server:cast(?MODULE, {broadcast, Msg}).

broadcast(Msg, Table) ->
  List = ets:match(Table, { '_', '$1'}),
  Pids = [ Pid || [Pid] <- List ],
  lists:map( fun(Pid) -> Pid ! Msg end, Pids).

create_scenario() ->
  gen_server:cast(?MODULE, create_scenario).

create_character(Cookie, Name) ->
  gen_server:cast(zhandler, {create_character, {Cookie, Name}}).
% Start gen_server functions


init([Port]) ->
  % trap_exit -> this gen_server needs to be supervised
  process_flag(trap_exit, true),
  % start misultin & set monitor
  misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]),
  erlang:monitor(process, misultin),
  ConnectionTable = ets:new(connections, [set]),
  ScenarioList = [],
  {ok, {ConnectionTable, ScenarioList}}.

% State is {ets_table_pid, list_of_scenario_pids).

handle_call(get_table, _From, State) ->
  {Table, _} = State,
  {reply, Table, State};

handle_call(Request, _From, State) ->
  io:format("~p~n", [Request]),
  {reply, undefined, State}.

handle_cast({broadcast, Msg}, {Table, _} = State) ->
  broadcast(Msg, Table),
  {noreply, State};

handle_cast(create_scenario, {Table, List}) ->
  {ok, NewScenario} = scenario:start(),
  NewList = lists:append(List, [NewScenario]),
  {noreply, {Table, NewList}};

handle_cast({create_character, {Cookie, Name}}, State) ->
  {_Table, [Scenario]} = State,
  {ok, Character} = character:create([Name, Scenario]),
  %ets:insert(Table, {Cookie, Character, inactive}),
  gen_server:cast(?MODULE, {update_table, {character_address, {Cookie, Character}}}),
  {noreply, State};

handle_cast({update_table, {Type, {Cookie, Address}}}, State) ->
  {Table, _} = State,
  case ets:member(Table, Cookie) of
    false ->
      ets:insert(Table, {Cookie, inactive, inactive});
    true ->
      ok
  end,
  case Type of
    character_address -> 
      ets:update_element(Table, Cookie, {2, Address});
    return_address -> 
      [{_, Character, _}] = ets:lookup(Table, Cookie),
      gen_server:cast(Character, {return_address, Address}),
      % I don't know if I'll actually need the next line, but I wrote it first,
      % so I don't need to take it out unless we realize it's completely
      % unnecessary.
      ets:update_element(Table, Cookie, {3, Address})
  end,
  {noreply, State};

handle_cast(stop, State) ->
  {stop, normal, State};

% handle_cast generic fallback (ignore)
handle_cast(Msg, State) ->
  io:format("~p~n", [Msg]),
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

 
 

% Start Misultin Handlers


handle_http(Req) ->
  %io:format("~p~n", [Req]),
  %io:format("~p - ~p~n", [Req:get(method), Req:resource([lowercase, urldecode])]),
  %io:format("~p~n", [Req:get(headers)]),
  case lists:keyfind('Cookie', 1, Req:get(headers)) of
    false -> 
      handle(nada, nada, nada, Req);
    {'Cookie', Cookie} ->
      handle(Req:get(method), Req:resource([lowercase, urldecode]), Cookie, Req)
  end.

handle('GET', [], Cookie, Req) ->
  Table = gen_server:call(?MODULE, get_table),
  InTable = ets:member(Table, Cookie),
  case InTable of
    true -> 
      Result = ets:lookup(Table, Cookie),
      [{_, Character, _}] = Result,
      case Character of
        inactive ->
          io:format("~p~n", ["This probably wont ever show..."]),
          Req:file("menu.html");
        Character ->
          Req:file("game.html")
      end;
    false -> 
      %io:format("~p~n", [ets:tab2list(Table)]),
      Req:file("menu.html")
  end;

handle('POST', [], Cookie, Req) ->
  Name = proplists:get_value("name", Req:parse_post()),
  create_character(Cookie, Name),
  handle('GET', [], Cookie, Req);

handle('POST', ["data"], Cookie, Req) ->
	Req:respond(204, [], ""),
  Params = Req:parse_post(),
  Table = gen_server:call(?MODULE, get_table),
  Result = ets:lookup(Table, Cookie),
  [{_, Character, _}] = Result,
  lists:foreach(fun(Param) -> gen_server:cast(Character, {post, Param}) end, Params);

handle('GET', ["data"], Cookie, Req) ->
  gen_server:cast(?MODULE, {update_table, {return_address, {Cookie, self()}}}),
  receive
    Msg ->
      gen_server:cast(?MODULE, {update_table, {return_address, {Cookie, inactive}}}),
      Req:respond(200, [{"Content-Type", "text/html"}], ["<div id='data'>", Msg, "</div>"])
  end;

handle(_,_,_,Req) ->
	Req:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").


% End Misultin Handlers
