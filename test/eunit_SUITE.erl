-module(eunit_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).


run_eunit(_Config) ->
    {ok, _} = application:ensure_all_started(erlog),
    ok = eunit:test([erlog_halt_tests]),
    ok = eunit:test([records_test]),
    pass.

all() -> [run_eunit].
