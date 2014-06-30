-module(disk_io_worker).

-behaviour(gen_server).

-export([start_link/1,stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
handle_info/2, code_change/3, terminate/2]).
 
-define(DELAY, 10500).
 
start_link(_) ->
	gen_server:start_link(?MODULE, [], []).

init([]) -> 
	{ok,[]}.
 
stop(_) -> gen_server:call(?MODULE, stop).


% call to cache a file
handle_call({process_file,Dir, F},_From, S) ->
	error_logger:info_report("this worker is going to process "++F),
	rpmr:cache_package(Dir,F),	
	case supervisor:start_child(scout, {Dir, {scout, start_link,[Dir]}, transient, brutal_kill, worker, [scout]}) of
		{ok,_} -> ok;
        {error,{already_started,P} } -> error_logger:info_report("casting message to "++io_lib:format("~p",[P])),
                                        gen_server:cast(P,Dir);
        _ -> ok
    end,

	{reply,ok, S};

% call to build repo from cache
handle_call({build_dir,D},_From, S) ->
	error_logger:info_report("this worker is going to build repo from cache in "++D),
	rpmr:generate_repo(cached, D),
    {reply,ok, S};

% call to process through all rpm files in a dir and verify cache
handle_call({process_dir,D},_From, S) ->
	error_logger:info_report("this worker is going to look through all packages in "++D),
	rpmr:generate_repo(direct, D),
	{reply, ok, S};

% handle a request to stop
handle_call(stop, _From, S) -> {stop, "Call", ok, S};

% a generic call handler. Caller should crash after sending this call
handle_call(Message, _From, S) ->
	error_logger:info_report("got call "++io_lib:format("~p",[Message])),
	{noreply, S}.

handle_cast(M, S) ->
	error_logger:info_report("got cast "++io_lib:format("~p",[M])),
	{noreply, S, ?DELAY}.

handle_info(_,_) -> ok.
code_change(_,_,_) -> ok.
terminate(_,_) -> ok.
