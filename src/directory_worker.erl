-module(directory_worker).
-compile(export_all).
-behavior(gen_server).
-export([init/1,terminate/2,handle_cast/2,handle_info/2, handle_call/3, code_change/3]).

-record(state, {base, table, descriptors, waiting, options}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) -> gen_server:cast(self(),{start}), {ok,#state{options=Args}}.

handle_call(_Request, _From, State) -> {reply,ok,State}.
handle_cast({start},State) -> look_for_and_load_cache(State),
							  Dirs=subdirs(proplists:get_value(directory,State#state.options)), 
							  start_watch(Dirs,State),
							  check_and_update_cache(State,Dirs), 
							  {noreply,State};

handle_cast(Msg, State) -> io:fwrite("got cast: ~p~n",[Msg]), {noreply, State}.
handle_info(Info, State) -> io:fwrite("got info: ~p~n",[Info]),{noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

look_for_and_load_cache(State) -> Table = filename:join([
											proplists:get_value(basedir,State#state.options,"/"),
											proplists:get_value(dir,State#state.options,"."),
											proplists:get_value(cache_filename,State#state.options, "watchbasedir.dat")
										]).
								  %dets:open_file(

start_watch(Directory, _State) -> 
	lists:foreach(fun(A) -> inoteefy:watch(A,fun(X) -> io:fwrite("~p~n",[X]) end) end, subdirs(Directory)).

preprocess(Directory) -> ok.

inotify_callback(X) -> io:write("got ~p~n",[X]).

check_and_update_cache(State,Dirs) -> ok.


subdirs(Directory) -> subdirs(Directory, []).

subdirs(Directory, Accumulator) ->
	io:format("processing ~s: ~n", [Directory]),
	case file:list_dir(Directory) of 
		{ok, List} -> lists:foldl(fun subdirs/2,[Directory|Accumulator], lists:filtermap(fun("repodata") -> false; 
																							(A) -> {true,filename:join(Directory,A)} end,
																						List));
		{error, _Reason} -> Accumulator
	end.


