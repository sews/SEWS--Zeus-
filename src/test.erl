%% Lägg alla tester i denna modul
%% Alla tester bör avslutas med _test

-module(test).
-export([]).

-include_lib("eunit/include/eunit.hrl").

mall_test() ->
	?_assertEqual(true, true).
