-module(erlog_mailbox_tests).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("erlog_test.hrl").

make_erlog() ->
    application:set_env(erlog, consult_path, [".", "../stdlib"]),
    {ok, ERLOG}			= erlog:new(),
    erlog:consult("erlang.pl",ERLOG).

prop_send() ->
    ?FORALL(Msg,
	    {word,non_empty(list(choose(65,90)))},
	    begin
		{ok, ERLOG1}                    = make_erlog(),
		{{succeed, _R}, _ERLOG2}	= erlog:prove({send, self(), Msg},ERLOG1),
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
		
		{{succeed, R}, _ERLOG2}	= erlog:prove({'receive',{'Msg'}, 500},ERLOG1 ),
		Msg =:= proplists:get_value('Msg', R)
	    end).

recieve_after_test() ->			     
    {ok, ERLOG1}                    = make_erlog(),
    ?assertMatch({fail, _}, erlog:prove( {'receive', {'Msg'}, 5}, ERLOG1)),
    true.
	    

