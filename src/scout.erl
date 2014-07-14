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
	lager:debug("Scout saw "++Name++". Waiting for some time"),
	{ok, #state{name=Name,dir=Dir}, ?DELAY};
init({_Dir,_Type,OP,_Cookie,Name}) -> 
	lager:warning("scout is ignoring operation " ++io_lib:format("~p",[OP])++" on "++Name),
	{ignore};
init(Dir) when is_list(Dir); is_binary(Dir) -> 
	lager:debug("Scout got directory " ++ [Dir] ++". Wating for timeout"),
	{ok, #state{dir=Dir},?DELAY};
	
init(_) -> 
	lager:error("Scout got wrong initialisation!"),
	{ignore}.
 
stop(_) -> 
	lager:debug("Scout is stopping by request"),
	gen_server:call(?MODULE, stop).

% someone called us and should die it's own way later
handle_call(M,F,State) -> 
	lager:error("Scout got message ~p from ~p, but it shouldn't!", [M,F]),
	{noreply, State, ?DELAY}.

% delay file or directory processing if they were changed within configured delay
handle_cast(Message, State) when State#state.dir==Message -> 
	lager:debug("Scout got request to build ~p  while already waiting for it", [Message]),
	{noreply, State, ?DELAY}; 
handle_cast({Dir,_Type,_OP,_Coockie,Name}, State) when State#state.name == Name, State#state.dir == Dir ->
	lager:debug("Scout got request to cache ~p ,while already waiting for it", [Name]),
	{noreply, State, ?DELAY};
handle_cast(Message, State) -> 
	lager:warning("Scout got ~p while waiting for ~p~p", [Message,State#state.dir,State#state.name]), 
	{noreply, State, ?DELAY}. 


handle_info(timeout,State=#state{name=""}) -> 
	lager:info("Scout calling poolboy to build repo in ~p", [State#state.dir]),
    poolboy:transaction(disk_workers, fun(Worker) -> gen_server:call(Worker,{build_dir,State#state.dir},600000) end, 600000),
	lager:debug("Scout got repo ~p built in time", [State#state.dir]),
    {stop,shutdown,State};

handle_info(timeout,State) when State#state.name /= "" , State#state.dir /= "" -> 
	lager:debug("Scout calling poolboy to cache ~p ~p",[State#state.dir,State#state.name]),
	poolboy:transaction(disk_workers, fun(Worker) -> gen_server:call(Worker,{process_file,State#state.dir,State#state.name},60000) end, 60000),
	lager:debug("Scout got package  ~p ~p cached in time",[State#state.dir,State#state.name]),
	{stop,shutdown,State};

handle_info(timeout,State) ->
	lager:error("Scout got timeout without arguments"),
	{stop,error,State}.

code_change(_,_,_) -> {error,"No code upgrade supposed for scout"}.
terminate(_,_) -> ok.
