-module(fs_call_fsm).
-behaviour(gen_statem).

-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, answered/3, bridged/3, hangup/3]).

-record(state, {uuid, data = #{}}).

start_link(UUID) ->
    gen_statem:start_link(?MODULE, UUID, []).

init(UUID) ->
    io:format("[fs_call_fsm] Start FSM for ~s~n", [UUID]),
    {ok, idle, #state{uuid = UUID}}.

callback_mode() -> state_functions.

idle({cast, {event, E}}, _, S=#state{uuid=UUID}) ->
    case maps:get(<<"Event-Name">>, E, <<"NONE">>) of
        <<"CHANNEL_CREATE">> ->
            io:format("[FSM:~s] Answering call~n", [UUID]),
            fs_util:send_cmd(UUID, "answer", ""),
            {next_state, answered, S};
        _ -> {keep_state_and_data, S}
    end.

answered({cast, {event, E}}, _, S=#state{uuid=UUID}) ->
    case maps:get(<<"Event-Name">>, E, <<"NONE">>) of
        <<"CHANNEL_ANSWER">> ->
            io:format("[FSM:~s] Bridging call to 1001~n", [UUID]),
            fs_util:send_cmd(UUID, "bridge", "user/1001"),
            {next_state, bridged, S};
        _ -> {keep_state_and_data, S}
    end.

bridged({cast, {event, E}}, _, S=#state{uuid=UUID}) ->
    case maps:get(<<"Event-Name">>, E, <<"NONE">>) of
        <<"CHANNEL_HANGUP">> ->
            io:format("[FSM:~s] Call ended~n", [UUID]),
            {next_state, hangup, S};
        _ -> {keep_state_and_data, S}
    end.

hangup(_,_,S) ->
    {stop, normal, S}.


code_change(_,_,_,_) -> {}.
terminate(_,_,_) -> {}.

