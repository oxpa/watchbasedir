-module(watchbasedirapp).
-behaviour(application).
-behaviour(supervisor).
-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).                                                                                                                                                                            
start() ->
    application:start(?MODULE).
stop() ->
    application:stop(?MODULE).
start(_Type, Args) ->
    supervisor:start_link(?MODULE, Args).
stop(_State) ->
    ok.
init(Args) ->
    Pools=[{name,{local,disk_workers}},{worker_module,disk_io_worker},{size,5},{max_overflow,0}],

    {ok, {{one_for_all, 1, 1}, [
        poolboy:child_spec(workers,Pools),
        {scouts, {watchbasedir_sup, start_link, []}, permanent, 180000, supervisor,[watchbasedir_sup]},
		{watch, {watchbasedir, start_link, [[]]}, permanent, 20000, worker, [watchbasedir]}
        ]}}.
