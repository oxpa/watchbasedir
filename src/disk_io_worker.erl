-module(disk_io_worker).

-behaviour(gen_server).

-export([start_link/1,stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
handle_info/2, code_change/3, terminate/2]).
 
-define(DELAY, 10500).
 
start_link(_) ->
	gen_server:start_link(?MODULE, [], []).

init([]) -> 
	lager:info("starting worker ~p", [self()]),
	process_flag(trap_exit, true),%to finish any file processin that might be in process
	{ok,[]}.
 
stop(_) -> gen_server:call(?MODULE, stop).


% call to cache a file
handle_call({process_file,Dir, F},_From, S) ->
	lager:info("Worker ~p is going to cache ~p",[self(),F]),
	rpmr:cache_package(Dir,F),	
	case supervisor:start_child(scout, {Dir, {scout, start_link,[Dir]}, transient, brutal_kill, worker, [scout]}) of
		{ok,_} -> ok;
        {error,{already_started,P} } -> gen_server:cast(P,Dir);
        _ -> lager:critical("Unable to start scout for ~p",[Dir])
    end,
	{reply, ok, S};

% call to build repo from cache
handle_call({build_dir,D},_From, S) ->
	lager:info("Worker is going to build repo from cache at ~p",[D]),
	rpmr:generate_repo(cached, D),
    {reply,ok, S};

% call to process through all rpm files in a dir and verify cache
handle_call({process_dir,D},_From, S) ->
	lager:info("Worker ~p is going to cache all packages in ~p",[self(),D]),
	rpmr:generate_repo(direct, D),
	{reply, ok, S};

% handle a request to stop
handle_call(stop, From, S) -> 
	lager:info("Worker ~p is stopping by request from ~p", [self(),From]),
	{stop, "Call", ok, S};

% a generic call handler. Caller should crash after sending this call
handle_call(Message, From, S) ->
	lager:error("Worker ~p got unexpected message ~p from ~p",[self(),Message, From]),
	{noreply, S}.

handle_cast(M, S) ->
	lager:error("Worker ~p got unexpected message ~p",[self(),M]),
	{noreply, S}.

handle_info({'EXIT',_,shutdown},S) -> 
	lager:debug("Worker ~p got EXIT and actually quiting",[self()]),
	{stop,shutdown,S};
handle_info(I,S) -> 
	lager:error("Worker got unexpected information ~p",[I]),
	{noreply,S}.
code_change(_,_,_) -> {stop,"No code upgrade supposed for worker"}.
terminate(_,_) -> lager:debug("Worker ~p teminating",[self()]),ok.
