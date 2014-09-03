-module(erlog_io_tests).

%% Copyright (c) 2014 Zachary Kessin
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("erlog_test.hrl").

prop_display_1() ->
    ?FORALL(Value,
	    oneof([int(),list(int()), binary(),atom]),
	    begin
		Leader      = self(),
		{ok, Erlog} = erlog:new(),
		Pid = spawn_link(fun() ->
					 erlang:group_leader(Leader, self()),
					 erlog:prove(Erlog,{display, Value}),
					 ok
				 end),
		receive
		    {io_request,Pid,_,{put_chars, unicode, io_lib, format, ["~p\n",[IO]]}} ->
			?assertEqual(Value, IO),
			true
		end
	    end).

nl_test() ->
    Leader      = self(),
    {ok, Erlog} = erlog:new(),
    Pid = spawn_link(fun() ->
			     erlang:group_leader(Leader, self()),
			     erlog:prove(Erlog, nl),
			     ok
		     end),
    receive
	{io_request,Pid,_,{put_chars, unicode, io_lib, format, ["\n",[]]}} ->
	    true
    after 50 ->
	    error("no message")
    end.

prop_display_2() ->
    ?FORALL({Format,Value},
	    oneof([{"~p",int()},
		   {"~s",list(choose(65,90))},
		   {"~s", binary()},
		   {"~p", binary()},
		   {"~s",atom}
		  ]),
	    begin
		Leader      = self(),
		{ok, Erlog} = erlog:new(),
		Pid = spawn_link(fun() ->
					 erlang:group_leader(Leader, self()),
					 erlog:prove(Erlog,{display, Format,Value}),
					 ok
				 end),
		receive
		    {io_request,Pid,_,{put_chars, unicode, io_lib, format, [Format,[IO]]}} ->
			?assertEqual(Value, IO),
			true;
		    R ->
			?debugFmt("R ~p",[R]),
			false
		after 50 ->
			false
		end
	    end).
	
