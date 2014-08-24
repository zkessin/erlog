-module(erlog_mailbox).

-include("erlog_int.hrl").

-compile(export_all).


send(Pid, Msg) ->
    Pid ! Msg,
    {succeed_last,Msg}.
