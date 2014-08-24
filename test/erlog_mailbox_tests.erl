-module(erlog_mailbox_tests).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("erlog_test.hrl").


prop_send() ->
    ?FORALL(Msg,
	    {word,non_empty(list(choose(65,90)))},
	    begin
		{ok, ERLOG}			= erlog:new(),
		{ok, ERLOG1}			= erlog:consult(ERLOG,"../priv/mailbox.pl"),
		{{succeed, _R}, _ERLOG2}	= erlog:prove(ERLOG1, {send, self(), Msg}),
		receive
		    Msg ->
			true
		after 100 ->
			false
		end
	    end).
