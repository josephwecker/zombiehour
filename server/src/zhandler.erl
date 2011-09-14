% ZHandler Module
% Misultin handler for all client to server communication
% 

-module(zhandler).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([broadcast/1, start/1, stop/0]).

% Module Functions
start(Attrs) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Attrs, []).

stop() ->
	gen_server:cast(?MODULE, stop).

broadcast(Msg) -> 
  gen_server:cast(?MODULE, {broadcast, Msg}).

broadcast(Msg, Table) ->
  List = ets:match(Table, { '_', '$1'}),
  Pids = [ Pid || [Pid] <- List ],
  lists:map( fun(Pid) -> Pid ! Msg end, Pids).

create_scenario(Name) ->
  gen_server:cast(?MODULE, {create_scenario, Name}).

create_character(Cookie, {Name, Class}, Scenario) ->
  gen_server:cast(zhandler, {create_character, {Cookie, {Name, Class}, Scenario}}).

% Gen Server Functions
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

handle_call(get_state, _From, State) ->
  {reply, State, State};

handle_call(Request, _From, State) ->
  io:format("~p~n", [Request]),
  {reply, undefined, State}.

handle_cast({broadcast, Msg}, {Table, _} = State) ->
  broadcast(Msg, Table),
  {noreply, State};

handle_cast({create_scenario, Name}, {Table, List}) ->
  {ok, NewScenario} = scenario:start(),
  NewList = [{Name, NewScenario}|List],
  {noreply, {Table, NewList}};

handle_cast({create_character, {Cookie, {Name, Class}, Scenario}}, State) ->
  {_Table, ScenarioList} = State,
  {Scenario, Pid} = lists:keyfind(Scenario, 1, ScenarioList),
  Character = gen_server:call(Pid, {create_character, {Name, Class}}),
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

%{ConnectionTable, ScenarioList}
handle_cast({close_scenario, Pid}, {T,ScenarioList}) ->
  NewList = lists:keydelete(Pid, 2, ScenarioList),
  {noreply, {T,NewList}};

handle_cast({close_character, Pid}, {Table, _} = State) ->
  [[Cookie]] = ets:match(Table, {'$1', Pid, '_'}),
  gen_server:cast(?MODULE, {update_table, {character_address, {Cookie, inactive}}}),
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

% Misultin Handlers
handle_http(Req) ->
  case lists:keyfind('Cookie', 1, Req:get(headers)) of
    false -> 
      handle(nada, nada, nada, Req);
    {'Cookie', Cookie} ->
      handle(Req:get(method), Req:resource([lowercase, urldecode]), Cookie, Req)
  end.

handle('GET', [], Cookie, Req) ->
  {Table, List} = gen_server:call(?MODULE, get_state),
  InTable = ets:member(Table, Cookie),
  case InTable of
    true -> 
      Result = ets:lookup(Table, Cookie),
      [{_, Character, _}] = Result,
      case Character of
        inactive ->
          menu(Req, List);
        Character ->
          Req:file("game.html"),
          timer:sleep(1000),
          gen_server:cast(Character, update_all)
      end;
    false -> 
      %io:format("~p~n", [ets:tab2list(Table)]),
      menu(Req, List)
  end;

handle('POST', ["create_scenario"], _Cookie, Req) ->
  Name = proplists:get_value("name", Req:parse_post()),
  create_scenario(Name),
  Req:respond(302, [{"Location", "/game/"}], "");

handle('POST', ["join"], Cookie, Req) ->
  Params = Req:parse_post(),
  Name = proplists:get_value("name", Params),
  Class = list_to_atom(proplists:get_value("class", Params)),
  Scenario = proplists:get_value("scenario", Params),
  create_character(Cookie, {Name, Class}, Scenario),
  Req:respond(302, [{"Location", "/game/"}], "");
  %handle('GET', [], Cookie, Req);

handle('POST', ["data"], Cookie, Req) ->
	Req:respond(204, [], ""),
  Params = Req:parse_post(),
  {Table, _} = gen_server:call(?MODULE, get_state),
  Result = ets:lookup(Table, Cookie),
  [{_, Character, _}] = Result,
  lists:foreach(fun(Param) -> gen_server:cast(Character, {post, Param}) end, Params);

handle('GET', ["data"], Cookie, Req) ->
  gen_server:cast(?MODULE, {update_table, {return_address, {Cookie, self()}}}),
  receive
    Msg ->
      gen_server:cast(?MODULE, {update_table, {return_address, {Cookie, inactive}}}),
      Req:respond(200, [{"Content-Type", "application/json"}], [Msg])
  end;

handle(_,_,_,Req) ->
	Req:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").

% Menu Template
menu(Req, List) ->
  Menu = case List of
    [] ->
            "<h2>Create a new Scenario</h2>
            <p>There isn't currently a game running, but you can start one by
            entering a scenario name:</p>
            <form action=\"create_scenario\" method=\"post\">
              Scenario name: <input type=\"text\" name=\"name\" />
              <input type=\"submit\" value=\"Create Scenario\" />
              </form>";
    List ->
  lists:append("<h2>The following games are available to join:<\h2>",
      lists:map(
        fun({Name, _}) ->
            lists:concat(["<h3>",Name,":</h3>
              <form action=\"join\" method=\"post\">
                <input type=\"hidden\" name=\"scenario\" value=\"",Name,"\" />
                <input type=\"radio\" name=\"class\" value=\"soldier\" checked=\"checked\"/> Soldier <br />
                <input type=\"radio\" name=\"class\" value=\"engineer\" /> Engineer <br />
                <input type=\"radio\" name=\"class\" value=\"medic\" /> Medic <br />
                <input type=\"radio\" name=\"class\" value=\"brawler\" /> Brawler <br />
                Character's name: <input type=\"text\" name=\"name\" value=\"Unnamed\" />
                <input type=\"submit\" value=\"Join Scenario\" />
                </form>"])
        end,
        List))
  end,
  Template = "<html>
  <head>
    <title>Zombie Hour</title>
    <link rel='stylesheet' type='text/css' href='../stylesheets/main.css' />
    <link rel='stylesheet' type='text/css' href='../stylesheets/fonts/stylesheet.css' />

  </head>
  <body>
    <div class=\"title\">
      <h1>The Zombie Hour</h1>
      <div id=\"sys_nav\">This doesn't work yet: <a href=\"\">Login</a></div>
    </div>
    <div>
      <div id=\"content\">
        <p>Welcome</p>
        <p>If you haven't played Zombie Hour before make sure to read the <a
        href=\"/controls.html\">Controls</a> page, before starting.</p>
        <p>You're not currently in a game.</p>
        ~s
      </div>
    </div>

  </body>
</html>",
  Req:ok([{"Content-Type", "text/html"}], Template, [Menu]).
