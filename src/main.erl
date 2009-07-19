%%%-------------------------------------------------------------------
-module(main).

-behaviour(gen_server).


%% API
-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(combination,
	{words,
	 state}).

-define(M(S, X), io:format("master: " ++ S, X)).
-define(W(S, X), io:format("worker ~p on ~p: " ++ S, [self(), node()] ++ X)).

-define(SERVER, headserv).

-record(state,
	{dict = [],
	 best = undefined,  %% {Phrase, Score}
	 workers = dict:new(),
	 max = 1}).


start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?M("MASTER ~p~n", [self()]),
    crypto:start(),
    process_flag(trap_exit, true),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    Res = mnesia:create_table(combination, [{disc_copies, [node()]}, 
					    {attributes, record_info(fields, combination)}]),
    ?M("RESULT ~p~n", [Res]),
    pool:start(worker, "-rsh ssh"),

    Module = main,
    {_Module, Binary, Filename} = code:get_object_code(Module),
    LoadingCode = rpc:multicall(code, load_binary, [Module, Filename, Binary]),
    ?M("Loading code: ~p~n", [LoadingCode]),

    ?M("Distributing executable... ", []),
    {Test, []} = rpc:multicall(os, cmd, ["test -f /tmp/sha1 || echo error"]),
    ?M("Done ~p~n", [Test]),
    [ "" = X || X <- Test ],

    {ok, Dictionary0} = file:read_file("priv/dict.txt"),
    Dictionary = [ list_to_binary(X) || X <- string:tokens(binary_to_list(Dictionary0), " \n") ],
    Workers = lists:foldl(fun(X, D) -> dict:store(X, 0, D) end, dict:new(), pool:get_nodes()),
    gen_server:cast(self(), start),
    {ok, #state{dict = Dictionary, workers = Workers}}.

handle_call({get, Pid}, _From, #state{dict = Dictionary} = State) ->
    ?M("~p on ~p requested combination~n", [Pid, node(Pid)]),
    Combination = get_new_combination(Dictionary),
    ?M("Combination for ~p on ~p~n", [Pid, node(Pid)]),
    mnesia:dirty_write(#combination{words = Combination, state = reserved}),
    {reply, {ok, Combination}, State}.

handle_cast({done, _Pid, Combination, NewBest}, #state{best = OldBest} = State) ->
    Best = better(OldBest, NewBest),	    
    mnesia:dirty_write(#combination{words = Combination, state = done}),
    ?M("New best is ~p~n", [OldBest]),
    {noreply, State#state{best = Best}};

handle_cast(start, State0) ->
    process_flag(trap_exit, true),
    State = add_workers(State0),
    ?M("~p~n", [State]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.




handle_info({'EXIT', Pid, normal}, State) ->
    ?M("pid ~p on ~p stopped~n", [Pid, node(Pid)]),
    {noreply, State};
handle_info({'EXIT', Pid, more}, State) ->
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


get_new_combination(Dictionary) ->
    Ar = array:from_list(Dictionary),
    S = array:size(Ar),
    lists:sort(get_new_combination0(Ar, S)).

get_new_combination0(Dictionary, S) ->
    N = crypto:rand_uniform(0, 13),
    Comb = [ [array:get(crypto:rand_uniform(0, S), Dictionary), " "] || _ <- lists:seq(1, N) ],
    case mnesia:dirty_read(combination, Comb) of
	[] ->
	    Comb;
	[_] ->
	    get_new_combination0(Dictionary, S)
    end.

worker_loop0(MasterNode) ->
    ?W("started, masternode ~p~n", [MasterNode]),
    worker_loop(MasterNode).

worker_loop(MasterNode) ->

    ?W("will send a request for job~n", []),
    {ok, Combination} = gen_server:call({?SERVER, MasterNode}, {get, self()}),
    Port = open_port({spawn, "/tmp/sha1"}, [{line, 1000},
					    use_stdio]),
%%     receive
%% 	{'EXIT', Port, Error} ->
%% 	    ?W("Port is dead ~p~n", [Error]),
%% 	    exit(Error)
%%     after
%% 	200 ->
%% 	    ok
%%     end,
    port_command(Port, [Combination, "\n"]),
    ?W("waiting for job to finish~n", []),
    Line = receive
	       {Port, {data, {eol, L}}} ->
		   ?W("Got ~p~n", [L]),
		   L;
	       _X -> 
		   ?W("Received ~p~n", [_X]),
		   exit(badmsg)
	   after
	       10000 ->
		   exit(timeout)
	   end,					     
    [Phrase, Score] = string:tokens(Line, "\t"),
    gen_server:cast({?SERVER, MasterNode}, {done, self(), Combination, {Phrase, list_to_integer(Score)}}),
    exit(more).

