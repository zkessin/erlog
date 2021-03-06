-module(erlog_int_tests).
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



prop_equal() ->
    ?FORALL({I,J},
	    {int(),int()},
	    begin
		{ok,E}   = erlog:new(),
	        case erlog:prove({'==',I,J},E) of
		    {fail,_} ->
			I =/= J;
		    {{succeed,[]},_} ->
			I == J
		end
	    end).

prop_not_equal() ->
    ?FORALL({I,J, Op},
	    {int(),int(), oneof(['\\==','\\='])},
	    begin
		{ok,E}   = erlog:new(),
	        case erlog:prove({Op,I,J},E) of
		    {fail, _} ->
			I == J;
		    {{succeed,[]},_} ->
			I =/= J
		end
	    end).

fail_test() ->
    {ok, ERLOG}   = erlog:new(),
    {fail,_}      = erlog:prove( fail,ERLOG),
    true.



keys() ->
    [
     "AAAAAAAA",
     "8BFE5E9B",
     "59665E9E",
     "D54BA0D0",
     "3A1D3C2A",
     "DB203B97",
     "EB77972F",
     "7445F8E0",
     "73547A12",
     "3820D3E8",
     "6EABF346",
     "EB75CC5E",
     "BA7F285E",
     "9882CB8F",
     "EA05A25E",
     "C125074F",
     "EC10B758",
     "54BB4C80",
     "537E16D9"].

bool_test() ->
    {ok,E}            = erlog:new(),
    {{succeed, []},_} =  erlog:prove(true,E),
%    {fail,_}          =  erlog:prove(E, false),
    {fail,_}          =  erlog:prove(fail,E),
    true.




option() ->
    oneof([assert, asserta, assertz]).
value() ->
    {model, elements(keys()), int()}.

prop_assert() ->
    ?FORALL({Op, Value},
            {option(), value()},
            begin
                {ok, ERLOG}   = erlog:new(),
                {{succeed,_},ERLOG1} = erlog:prove({Op, Value},ERLOG),
                case  erlog:prove( Value,ERLOG1) of
                    {{succeed,_},_} -> true;
                    _           -> false
                end
            end).

prop_retract() ->
    ?FORALL({Op, Value},
            {oneof([retract]), value()},
            begin
                {ok, ERLOG}		= erlog:new(),
                {{succeed,_},ERLOG1}	= erlog:prove({asserta, Value},ERLOG),
                {{succeed,_}, ERLOG2}	= erlog:prove( Value,ERLOG1),
                {{succeed,_}, ERLOG3}	= erlog:prove({Op, Value},ERLOG2),
                case  erlog:prove(Value,ERLOG3) of
                    {{succeed,_},_}  -> false;
                    {fail, _}        -> true
                end
            end).
              




