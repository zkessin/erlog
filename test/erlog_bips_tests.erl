-module(erlog_bips_tests).
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
-include("erlog_test.hrl").
-compile(export_all).


cops() ->
    oneof([{'=:=', fun (I,J) ->
			   I == J
		   end},
	   {'==', fun (I,J) ->
			   I == J
		   end},

	   {'=\\=', fun(I,J) ->
			   I /= J
		   end},
	   {'\\==', fun(I,J) ->
			   I /= J
		   end},

	   {'\\=', fun(I,J) ->
			   I /= J
		   end},

	   {'<', fun(I, J) ->
			 I < J
		 end},
	   {'>', fun(I,J) ->
			 I > J
		 end},
	   {'>=', fun(I, J) ->
			  I >= J
		  end},
	   {'=<', fun(I,J) ->
			  I =< J
		  end}]).

atom()->
    elements(['a','b','c','d','e','f','g','A',' ','_']).

is_atomic(A) when is_list(A) ->
    false;
is_atomic(A) when is_tuple(A) ->
    false;
is_atomic(_) ->
    true.

prop_atom() ->
    ?FORALL(MaybeAtom,
	    oneof([int(),atom()]),
	    begin
		{ok,Erlog}    = erlog:new(),
                case {erlog:prove( {atom,  MaybeAtom},Erlog), is_atom(MaybeAtom)} of
		    {{{succeed,_},_}, true} -> true;
		    {{fail,_}, false} -> true;
		    _  -> false
		end
	    end).


prop_is_integer() ->
    ?FORALL(MaybeInt,
	    oneof([int(),atom(), real()]),
	    begin
		{ok,Erlog}    = erlog:new(),
                case {erlog:prove( {integer,  MaybeInt},Erlog), is_integer(MaybeInt)} of
		    {{{succeed,_},_}, true} -> true;
		    {{fail,_}, false} -> true;
		    _  -> false
		end
	    end).


prop_atomic_and_compound() ->
    ?FORALL(Atom,
	    oneof([int(),atom(),real(),binary(),non_empty(list(int())),{atom(), int()}]),
	    begin
		{ok,Erlog}    = erlog:new(),
                case {erlog:prove( {atomic,  Atom}, Erlog),
		      erlog:prove( {compound,Atom},Erlog)}
		      of
		    {{{succeed,_},_},{fail,_}} ->
			is_atomic(Atom);
		    {{fail,_},{{succeed,_},_}} ->
			not(is_atomic(Atom))
		end
	    end).


prop_comp() ->
    ?FORALL({I, J, {Op,C}},
	    {oneof([int(),real()]), int(), cops()},
	    begin
                {ok, ERLOG}    = erlog:new(),
                case erlog:prove({Op, I, J},ERLOG ) of
		    {{succeed, _},_ } -> 
			C(I,J);
		    {fail, _} ->
			not(C(I,J))
		    end
		end).

any() ->
    oneof([int(),atom(), binary(),list(char())]).

prop_equals() ->
    ?FORALL(I, any(),
            begin
                {ok,Erlog}    = erlog:new(),
		?assertMatch({{succeed, [{'X',I}]},_}, erlog:prove({'=', I,     {'X'}},Erlog)),
		?assertMatch({{succeed, [{'X',I}]},_}, erlog:prove({'=', {'X'}, I},Erlog)),
                ?assertMatch({{succeed, []},_},        erlog:prove({'=', I,     I},Erlog)),
		true
            end).
prop_not_equals() ->
    ?FORALL({I,J}, {any(),any()},
	    ?IMPLIES(I /= J,
            begin
                {ok,Erlog}    = erlog:new(),
                ?assertMatch({fail,_}, erlog:prove({'=', I, J},Erlog)),
		true
            end)).

prop_float()->
    ?FORALL(I,real(),
            begin
                {ok, ERLOG}    = erlog:new(),
                {{succeed, _},_} = erlog:prove( {float, I},ERLOG),
                true
            end).

prop_integer()->
    ?FORALL(I,int(),
            begin
                {ok, ERLOG}    = erlog:new(),
                {{succeed, _},_} = erlog:prove({integer, I},ERLOG),
                true
            end).
prop_number()->
    ?FORALL(I,oneof([int(),real()]),
            begin
                {ok, ERLOG}    = erlog:new(),
                {{succeed, _},_} = erlog:prove({number, I},ERLOG),
                true
            end).



prop_arg() ->
    ?FORALL(T,
	    non_empty(list(oneof([binary(),int(), bool(), char(), real()]))),
	    ?FORALL(Place, choose(1,length(T)),
		    begin
			{ok,Erlog} = erlog:new(),
			P  = list_to_tuple([tuple|T]),

			{{succeed, [{'El',El}]},_} = erlog:prove({arg, Place, P, {'El'}},Erlog),
			?assertEqual(element(Place + 1, P), El),
			true

		    end)).


clause_test() ->
    {ok,E}		= erlog:new(),
    {ok, Erlog}            = erlog:consult("../stdlib/erlang.pl",E),
    {{succeed, A1},_E2}	= erlog:prove( {clause, {record, {'X'},{'Y'}}, {'Z'}}, Erlog),
  %  {{succeed, _A2},_}   = erlog:next_solution(E2),
    ?assertEqual(3,length(A1)),
 %   ?assertEqual(3,length(A2)),
    ?assertEqual('!', proplists:get_value('Z', A1)),
   % ?assertMatch({record,{_},{_},1}, proplists:get_value('Z',A2)),
    true.


