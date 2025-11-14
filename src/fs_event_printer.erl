-module(fs_event_printer).
-export([start/1]).

%% start(Node) spawns the loop for the given FS node
start(FSNode) ->
    loop(FSNode).

%% loop/1 receives events from FS and prints them
loop(FSNode) ->
    receive
        {event, Event} ->
            io:format("EVENT RECEIVED: ~p~n", [Event]),
            loop(FSNode);
        {nodedown, Node} ->
            io:format("FS Node down: ~p~n", [Node]),
            loop(FSNode);
        _Other ->
            loop(FSNode)
    end.

