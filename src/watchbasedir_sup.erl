-module(watchbasedir_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

% by default, this supervisor shouldn't start anything
% it will wait till requests from other processes
start_link() -> supervisor:start_link({local,scout},?MODULE,[]).
init(_) -> {ok, {{one_for_one, 5, 1}, []}}.

