-module(erlog_mailbox_tests).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("erlog_test.hrl").

make_erlog() ->
    {ok, ERLOG}			= erlog:new(),
    erlog:consult(ERLOG,"../priv/mailbox.pl").

prop_send() ->
    ?FORALL(Msg,
	    {word,non_empty(list(choose(65,90)))},
	    begin
		{ok, ERLOG1}                    = make_erlog(),
		{{succeed, _R}, _ERLOG2}	= erlog:prove(ERLOG1, {send, self(), Msg}),
		receive
		    Msg ->
			true
		after 100 ->
			false
		end
	    end).


prop_recieve() ->
    ?FORALL(Msg,
	    {word,non_empty(list(choose(65,90)))},
	    begin
		{ok, ERLOG1}                    = make_erlog(),
		self() ! Msg,
		
		{{succeed, R}, _ERLOG2}	= erlog:prove(ERLOG1, {'receive', {'Msg'}, 500}),
		Msg =:= proplists:get_value('Msg', R)

	    end).
			     

