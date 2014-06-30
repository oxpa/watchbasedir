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

init(Args) -> gen_server:cast(self(), start), {ok,#state{}}.

handle_cast(start, State) ->
	Args=[],
    Callback = fun ({Dir,_Type,_OP,_Coockie,Name}=A) ->
                %TODO: filter events by OP, Name and/or cookie
                case supervisor:start_child(scout, {filename:join(Dir,Name), {scout,start_link,[A]}, temporary, brutal_kill, worker, [scout]}) of
                    {ok,_} -> ok;
                    {error,{already_started,P} } -> error_logger:info_report("casting message to "++io_lib:format("~p",[P])),
                                                    gen_server:cast(P,A);
                    _ -> ok
                end end,
    Dirs=lists:filter(fun(Dir) -> filelib:is_dir(Dir) end, 
					  proplists:get_value(dirs,Args,["/home/oxpa/programms/watchbasedir/test.repo/Packages/"])),
	
	lists:foreach(fun(Dir) -> 
					filelib:ensure_dir(filename:join([Dir,"wbcache","watchbasedir.dat"])),
					dets:open_file(Dir,[{file,filename:join([Dir,"wbcache","watchbasedir.dat"])},{ram_file, true}]) end, Dirs),
    lists:foreach(fun(Dir) -> erlinotify:watch(Dir, Callback) end, Dirs),
    lists:foreach(fun(Dir) -> poolboy:transaction(disk_workers, fun(Worker) -> gen_server:call(Worker,{process_dir,Dir}, 600000) end, 600000) 
							end, Dirs),
    {noreply,#state{dirs=Dirs}};

handle_cast(M, S)  when M /= start->
    {noreply, S}.
handle_call(stop, _From, S) -> {stop, shutdown, S}.

handle_info(_,State) ->
    {noreply,State}.

code_change(_,_,_) -> ok.
terminate(_,State) -> lists:foreach(fun(Dir) -> dets:close(Dir) end, State#state.dirs),
				ok.

