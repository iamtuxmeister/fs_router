-module(fs_connection).
-behaviour(gen_server).

-export([start_link/0, send/1]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

-define(FS_NODE, 'freeswitch@127.0.0.1').

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    net_kernel:monitor_nodes(true),
    io:format("[fs_connection] Started. Waiting for ~p...~n", [?FS_NODE]),
    timer:send_interval(5000, ping),
    {ok, #{connected => false}}.

handle_info(ping, State) ->
    case net_adm:ping(?FS_NODE) of
        pong ->
            io:format("[fs_connection] Connected to FreeSWITCH.~n"),
            {noreply, State#{connected => true}};
        pang ->
            {noreply, State#{connected => false}}
    end;

handle_info({nodeup, ?FS_NODE}, State) ->
    io:format("[fs_connection] Node up: ~p~n", [?FS_NODE]),
    {noreply, State#{connected => true}};

handle_info({nodedown, ?FS_NODE}, State) ->
    io:format("[fs_connection] Node down: ~p~n", [?FS_NODE]),
    {noreply, State#{connected => false}};

handle_info({erlang, {event, Event}}, State) ->
    %% FreeSWITCH sends events as {erlang, {event, Map}}
    fs_event_handler:route_event(Event),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

send(Term) ->
    %% FreeSWITCH listens for {'erlang', Term} tuples.
    %% Example Term: {sendmsg, UUID, [{"call-command", "execute"}, ...]}
    erlang:send({erlang, ?FS_NODE}, Term).

handle_cast(_, S) -> {noreply, S}.
handle_call(_,_,S) -> {reply, ok, S}.
terminate(_,_) -> ok.
code_change(_,S,_) -> {ok,S}.

