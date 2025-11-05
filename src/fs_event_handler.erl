-module(fs_event_handler).
-behaviour(gen_server).

-export([start_link/0, route_event/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("[fs_event_handler] Ready.~n"),
    {ok, #{calls => #{}}}.

route_event(Event) ->
    gen_server:cast(?MODULE, {event, Event}).

handle_cast({event, #{<<"Unique-ID">> := UUID} = Event}, State) ->
    Calls = maps:get(calls, State),
    Pid = case maps:get(UUID, Calls, undefined) of
        undefined ->
            {ok, P} = fs_call_fsm:start_link(UUID),
            P;
        Existing -> Existing
    end,
    gen_statem:cast(Pid, {event, Event}),
    {noreply, State#{calls := maps:put(UUID, Pid, Calls)}};

handle_cast(_, State) -> {noreply, State}.
handle_call(_,_,S) -> {reply, ok, S}.
handle_info(_,S) -> {noreply,S}.
terminate(_,_) -> ok.
code_change(_,S,_) -> {ok,S}.

