-module(scout).

-behaviour(gen_server).

-export([start_link/1, start_link/0,stop/1]).
-export([init/1, code_change/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
 
-record(state, {name="", dir=""}).
-define(DELAY, 10500).


 
start_link() ->
	gen_server:start_link(?MODULE, [], []).
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init({Dir,_Type,close_write,_Cookie,Name}) ->
	error_logger:info_report("starting "++Name++" watch"),
	{ok, #state{name=Name,dir=Dir}, ?DELAY};
init({_Dir,_Type,OP,_Cookie,Name}) -> 
	error_logger:info_report("ignoring operation " ++io_lib:format("~p",[OP])++" on "++Name),
	{ignore};

init(Dir) when is_list(Dir); is_binary(Dir) -> {ok, #state{dir=Dir}};
	
init(_) -> {ignore}.
 
stop(_) -> gen_server:call(?MODULE, stop).

% someone called us and should die it's own way later
handle_call(M,_F,State) -> 
	error_logger:info_report("Got "++[M] ++" while shouldn't"),
	{noreply, State, ?DELAY}.

% delay file or directory processing if they were changed within configured delay
handle_cast(Message, State) when State#state.dir==Message -> {noreply, State, ?DELAY}; 
handle_cast({Dir,_Type,_OP,_Coockie,Name}, State) when State#state.name == Name, State#state.dir == Dir -> {noreply, State, ?DELAY};
handle_cast(Message, State) -> error_logger:info_report("Got " ++ [Message] ++ "while waiting for " ++ [State#state.dir] ++ [State#state.name]), 
								{noreply, State, ?DELAY}. 


handle_info(timeout,State=#state{name=""}) -> 
    error_logger:info_report("starting "++State#state.dir++" building from cache"),
    poolboy:transaction(disk_workers, fun(Worker) -> gen_server:call(Worker,{build_dir,State#state.dir}) end, 60000),
    {stop,shutdown,State};

handle_info(timeout,State) when State#state.name /= "" , State#state.dir /= "" -> 
	error_logger:info_report("starting "++State#state.name++" processing"),
	poolboy:transaction(disk_workers, fun(Worker) -> gen_server:call(Worker,{process_file,State#state.dir++State#state.name}) end, 60000),
	{stop,shutdown,State};

handle_info(timeout,State) ->
	error_logger:info_report("Scout got timeout without arguments"),
	{stop,error,State}.

code_change(_,_,_) -> ok.
terminate(_,_) -> ok.
