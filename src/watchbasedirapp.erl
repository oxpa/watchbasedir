-module(watchbasedirapp).
-behaviour(application).
-behaviour(supervisor).
-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).                                                                                                                                                                            

%% @doc Start the application. Mainly useful for using `-s watchbasedir' as a command
%% line switch to the VM to make lager start on boot.
start() -> start(watchbasedirapp).

start(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

stop() ->
    application:stop(?MODULE).
start(_Type, Args) ->
    supervisor:start_link(?MODULE, Args).
stop(_State) ->
    ok.
init(_Args) ->
	lager:set_loglevel(lager_console_backend, debug),
	lager:info("starting application"),
    Pools=[{name,{local,disk_workers}},{worker_module,disk_io_worker},{size,5},{max_overflow,0}],

    {ok, {{one_for_all, 1, 1}, [
        poolboy:child_spec(workers,Pools),
        {scouts, {watchbasedir_sup, start_link, []}, permanent, 180000, supervisor,[watchbasedir_sup]},
		{watch, {watchbasedir, start_link, [[]]}, permanent, 20000, worker, [watchbasedir]}
        ]}}.
