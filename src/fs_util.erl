-module(fs_util).
-export([send_cmd/3]).

send_cmd(UUID, App, Arg) ->
    Msg = {sendmsg,
           UUID,
           [
             {"call-command", "execute"},
             {"execute-app-name", App},
             {"execute-app-arg", Arg},
             {"event-lock", "true"}
           ]},
    fs_connection:send(Msg).

