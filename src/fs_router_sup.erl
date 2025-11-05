-module(fs_router_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {fs_connection, {fs_connection, start_link, []}, permanent, 5000, worker, [fs_connection]},
        {fs_event_handler, {fs_event_handler, start_link, []}, permanent, 5000, worker, [fs_event_handler]}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.

