%%%-------------------------------------------------------------------
-module(main).

-behaviour(gen_server).


%% API
-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(M(S, X), io:format("master: " ++ S, X)).
-define(W(S, X), io:format("worker ~p on ~p: " ++ S, [self(), node()] ++ X)).

-define(SERVER, headserv).

-record(state,
	{best = undefined,  %% {Phrase, Score}
	 workers = dict:new(),
	 max = 3}).


start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?M("MASTER ~p~n", [self()]),
    crypto:start(),
    process_flag(trap_exit, true),

    pool:start(worker, "-rsh ssh"),

    Module = main,
    {_Module, Binary, Filename} = code:get_object_code(Module),
    LoadingCode = rpc:multicall(code, load_binary, [Module, Filename, Binary]),
    ?M("Loading code: ~p~n", [LoadingCode]),

    ?M("Distributing executable... ", []),
    {Test, []} = rpc:multicall(os, cmd, ["test -f /tmp/sha1 || echo error"]),
    ?M("Done ~p~n", [Test]),
    [ "" = X || X <- Test ],
    
    Workers = lists:foldl(fun(X, D) -> dict:store(X, 0, D) end, dict:new(), pool:get_nodes()),
    gen_server:cast(self(), start),

    {ok, #state{workers = Workers}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({done, _Pid, NewBest}, #state{best = OldBest} = State) ->
    Best = better(OldBest, NewBest),	    
    ?M("New best is ~p~n", [OldBest]),
    {noreply, State#state{best = Best}};

handle_cast(start, State0) ->
    State = add_workers(State0),
    ?M("~p~n", [State]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.




handle_info({'EXIT', Pid, normal}, State) ->
    ?M("pid ~p on ~p FINISHED it's work~n", [Pid, node(Pid)]),
    {noreply, remove_worker(Pid, State)};
handle_info({'EXIT', Pid, Reason}, State) ->
    ?M("~p on ~p EXITed because ~p~n", [Pid, node(Pid), Reason]),
    {noreply, remove_worker(Pid, State)};
handle_info(_Info, State) ->
    ?M("UNKNOWN MESSAGE ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?M("TERMINATING ~p ~p~n", [self(), _Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





better(undefined, New) ->
    New;
better({_, ScoreO} = _O,
       {_, ScoreN} = N) when ScoreN < ScoreO ->
    N;
better(O, _) ->
    O.

add_workers(#state{workers = Workers0, max = Max} = State) ->
    L = lists:keysort(2, dict:to_list(Workers0)),
    %%?M("list ~p~n", [L]),
    Min = hd(L),
    case Min of
	{_Node, Max} ->
	    ?M("there is enough workers out there~n", []),
	    State;
	{Node, N} when N < Max ->
	    MasterNode = node(),
	    ?M("trying to start worker ~p~n", [Node]),
	    Pid = spawn_link(Node, fun() -> worker_loop0(MasterNode) end),
	    ?M("Started worker ~p on ~p~n", [Pid, Node]),
	    add_workers(State#state{workers = dict:update_counter(Node, 1, Workers0)})
    end.

remove_worker(Pid, #state{workers = Workers0} = State) ->
    add_workers(State#state{workers = dict:update_counter(node(Pid), -1, Workers0)}).


worker_loop0(MasterNode) ->
    ?W("started, masternode ~p~n", [MasterNode]),
    worker_loop(MasterNode).

worker_loop(MasterNode) ->
    ?W("starting sha1~n", []),
    open_port({spawn, "/tmp/sha1"}, [{line, 1000},
				     use_stdio]),
    ?W("waiting for messages from sha1~n", []),

    process_flag(trap_exit, true),
    
    worker_loop2(MasterNode).

worker_loop2(MasterNode) ->
    receive
	{_Port, {data, {eol, L}}} ->
	    [Score | Phrase] = string:tokens(L, " \t"),
	    gen_server:cast({?SERVER, MasterNode}, {done, self(), {Phrase, list_to_integer(Score)}}),
	    ?W("Got ~p~n", [L]);
	_X -> 
	    ?W("Received ~p~n", [_X]),
	    exit(badmsg)
    after
	2147483647 ->
	    exit(timeout)
    end,
    worker_loop2(MasterNode).

