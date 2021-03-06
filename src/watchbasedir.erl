-module(watchbasedir).

-behaviour(gen_server).

-export([start_link/1, start_link/0,stop/0]).
-export([init/1, handle_call/3, handle_cast/2,
handle_info/2, code_change/3, terminate/2]).

-record(state, {name="", dirs=[]}).
-define(DELAY, 10500).

start_link() ->
    gen_server:start_link(?MODULE, [], []).
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() -> gen_server:call(?MODULE, stop).

init(Args) -> 
	process_flag(trap_exit, true),
	gen_server:cast(self(), {start,Args}), {ok,#state{}}.

handle_cast({start,Args}, State) ->
						%TODO: filter events by OP, Name and/or cookie
    Callback = fun	({Dir,_Type,OP,_Coockie,Name}=A) when OP /= close_write -> ok;
					({Dir,_Type,OP,_Coockie,Name}=A) when OP == close_write ->               %TODO: restart strategy in configuration
						case supervisor:start_child(scout, {filename:join(Dir,Name), {scout,start_link,[A]}, temporary, brutal_kill, worker, [scout]}) of
							{ok,_} -> ok;
							{error,{already_started,P} } -> error_logger:info_report("casting message to "++io_lib:format("~p",[P])),
															gen_server:cast(P,A);
							_ -> ok
						end 
				end,
    Dirs=lists:filter(fun(Dir) -> filelib:is_dir(Dir) end, 
					  proplists:get_value(dirs,Args,["/home/oxpa/programms/watchbasedir/test.repo/Packages/"])),
	lager:debug("watchbasedir got dir list of: ~p",[Dirs]),
	lists:foreach(fun(Dir) -> 
					lager:debug("watchbasedir: opening cache for ~p",[Dir]),
					filelib:ensure_dir(filename:join([Dir,"wbcache","watchbasedir.dat"])),
					dets:open_file(Dir,[{file,filename:join([Dir,"wbcache","watchbasedir.dat"])},{ram_file, true}]) 
				  end, 
				  Dirs),
    lists:foreach(fun(Dir) -> erlinotify:watch(Dir, Callback) end, Dirs),
    lists:foreach(fun(Dir) -> 
							case supervisor:start_child(scout, {Dir, {scout, start_link,[{verify,Dir}]}, transient, brutal_kill, worker, [scout]}) of
								{ok,_} -> ok;
								{error,{already_started,P} } -> gen_server:cast(P,Dir);
								_ -> lager:critical("watchbasedir: unable to start scout for ~p",[Dir])
							end
						%poolboy:transaction(disk_workers, fun(Worker) -> gen_server:call(Worker,{process_dir,Dir}, 600000) end, 600000) 
				  end, Dirs),
    {noreply,#state{dirs=Dirs}};

handle_cast(M, S)  when M /= start->
    {noreply, S}.
handle_call(stop, _From, S) -> {stop, shutdown, S}.

handle_info(_,State) ->
    {noreply,State}.

code_change(_,_,_) -> ok.
terminate(_,State) -> lists:foreach(fun(Dir) -> dets:close(Dir) end, State#state.dirs),
				lager:debug("watchbasedir ~p terminating",[self()]),
				ok.

