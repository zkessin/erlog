-module(records_test).
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


-record(person, {name, phone, address, comments}).
name() ->
    elements(["Adam", "Bob","Charlie"]).

person() ->
    #person{name	= name(),
	    phone	= vector(1, choose(48,57)),
	    address	= list(char()),
	    comments	= binary()}.


prop_prolog_records_get() ->
    application:set_env(erlog, consult_path, [".", "../stdlib"]),
    ?FORALL(Person,
	    person(),
	    begin

                {ok,E}					= erlog:new(),
                {ok, E1}                                = erlog:consult("erlang.pl",E),
                Fields                                  = record_info(fields, person),
                {{succeed,_}, E2}                       = erlog:prove({record, person, Fields},E1),


                {{succeed,[{'Name', Name}]}, _ }        = erlog:prove( {person, name, Person, {'Name'}}, E2),
                ?assertEqual(Person#person.name, Name),
                {{succeed,[{'Phone', Phone}]}, _}       = erlog:prove( {person, phone, Person, {'Phone'}}, E2),
                ?assertEqual(Person#person.phone, Phone),
                {{succeed,[{'Address', Address}]}, _}   = erlog:prove( {person, address, Person, {'Address'}},E2),
                ?assertEqual(Person#person.address, Address),
                {{succeed,[{'Comments', Comments}]}, _} = erlog:prove( {person, comments, Person, {'Comments'}},E2),
                ?assertEqual(Person#person.comments, Comments),
                true
	    end).
 
prop_prolog_records_set() ->
    application:set_env(erlog, consult_path, [".", "../stdlib"]),
    ?FORALL({Person,NewName},
	    {person(),name()},
	    begin
                {ok,E} = erlog:new(),
                {ok, E1}                                = erlog:consult("erlang.pl",E),

                Fields                                  = record_info(fields, person),
                {{succeed,_}, E2}                       = erlog:prove({record, person, Fields}, E1),

		{{succeed,[{'Person', NewPerson }]},_} =
		    erlog:prove({person, name, Person, NewName, {'Person'}}, E2),
		?assert(is_record(NewPerson, person)),
		?assertEqual(NewPerson#person.name , NewName),

		{{succeed,[{'Person', NewPerson1 }]},_} =
		    erlog:prove({person, address, Person, NewName, {'Person'}},E2),
		?assertEqual(NewPerson1#person.address , NewName),
		true
	    end).
